{
  lib,
  stdenv,
  fetchFromGitHub,
  emacs30,
  autoreconfHook,
}:

let
  versionDate = "20260525";
  rev = "6d15d68e1f77ebb81827d792fbc67363dd5b730c";
in
(emacs30.override {
  srcRepo = true;
  withNativeCompilation = true;
  withNS = true;
  withTreeSitter = true;
  withXwidgets = true;
  withImageMagick = false;
  withMailutils = false;
}).overrideAttrs
  (old:
  let
    nativeCompilePostInstall = ''
      echo "Generating native-compiled trampolines..."
      # precompile trampolines in parallel, but avoid spawning one process per trampoline.
      # 1000 is a rough lower bound on the number of trampolines compiled.
      $out/bin/emacs --batch --eval "(mapatoms (lambda (s) \
        (when (subr-primitive-p (symbol-function s)) (print s))))" \
        | xargs -n $((1000/NIX_BUILD_CORES + 1)) -P $NIX_BUILD_CORES \
          $out/bin/emacs --batch -l comp --eval "(while argv \
            (comp-trampoline-compile (intern (pop argv))))"
      mkdir -p $out/share/emacs/native-lisp
      $out/bin/emacs --batch \
        --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp\")" \
        -f batch-native-compile $out/share/emacs/site-lisp/site-start.el
    '';
  in
  {
    pname = "emacs-weekly";
    version = "32.0.50-${versionDate}";

    src = fetchFromGitHub {
      owner = "emacs-mirror";
      repo = "emacs";
      inherit rev;
      hash = "sha256-ZI7fcoGR4d1rTnwtZg2mJNk6OtvpPMxBvlJMLYyY69U=";
    };

    patches = lib.filter (
      patch: lib.hasInfix "native-comp-driver-options" (builtins.baseNameOf (toString patch))
    ) (old.patches or [ ]);

    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ autoreconfHook ];

    postInstall = lib.replaceStrings [ nativeCompilePostInstall ] [ "" ] (old.postInstall or "");

    configureFlags =
      (old.configureFlags or [ ])
      ++ [
        "--enable-mac-self-contained"
      ];

    env = (builtins.removeAttrs (old.env or { }) [ "NATIVE_FULL_AOT" ]) // {
      NIX_CFLAGS_COMPILE = lib.optionalString stdenv.hostPlatform.isDarwin "-O3 -mcpu=native -mtune=native";
    };

    meta = (old.meta or { }) // {
      description = "Weekly GNU Emacs app build pinned from emacs-mirror";
      mainProgram = "emacs";
    };
  })
