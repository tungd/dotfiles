{ stdenv, fetchurl, ncurses, pkgconfig, texinfo, libxml2, gnutls, gettext, autoconf, automake
, AppKit, Carbon, Cocoa, IOKit, OSAKit, Quartz, QuartzCore, WebKit
, ImageCaptureCore, GSS, ImageIO # These may be optional
}:

stdenv.mkDerivation rec {
  name = "emacs-mac-snapshot";
  src = fetchurl {
    url = "https://bitbucket.org/mituharu/emacs-mac/get/work.tar.gz";
    sha256 = "ba398d1688148f7c0320e110a2f79d14c46584eb19bab84bc9489bd3f2e109a7";
  };

  enableParallelBuilding = true;

  nativeBuildInputs = [ pkgconfig autoconf automake ];
  buildInputs = [ ncurses libxml2 gnutls texinfo gettext ];
  propagatedBuildInputs = [
    AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit
    ImageCaptureCore GSS ImageIO   # may be optional
  ];

  preConfigure = ''
    ./autogen.sh
  '' + ''
    substituteInPlace lisp/international/mule-cmds.el \
      --replace /usr/share/locale ${gettext}/share/locale
    for makefile_in in $(find . -name Makefile.in -print); do
        substituteInPlace $makefile_in --replace /bin/pwd pwd
    done
  '';

  configureFlags = [ "--with-modules" "--with-gnutls" ]
    ++ [ "--with-mac" "--enable-mac-app=$$out/Applications" ];

  postInstall = ''
    cd mac
    make all
    mv Emacs.app $out/Applications/_Emacs.app
  '';

  meta = with stdenv.lib; {
    description = "The extensible, customizable GNU text editor, macOS port";
    homepage    = https://bitbucket.org/mituharu/emacs-mac;
    license     = licenses.gpl3Plus;
    platforms   = platforms.darwin;

    longDescription = ''
      GNU Emacs is an extensible, customizable text editor—and more.  At its
      core is an interpreter for Emacs Lisp, a dialect of the Lisp
      programming language with extensions to support text editing.
      The features of GNU Emacs include: content-sensitive editing modes,
      including syntax coloring, for a wide variety of file types including
      plain text, source code, and HTML; complete built-in documentation,
      including a tutorial for new users; full Unicode support for nearly all
      human languages and their scripts; highly customizable, using Emacs
      Lisp code or a graphical interface; a large number of extensions that
      add other functionality, including a project planner, mail and news
      reader, debugger interface, calendar, and more.  Many of these
      extensions are distributed with GNU Emacs; others are available
      separately.
    '';
  };
}
