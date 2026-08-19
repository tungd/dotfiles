#!/bin/zsh
set -euo pipefail

script_directory="${0:A:h}"
app_directory="${script_directory:h}"
cd "$app_directory"

xcodegen generate --spec project.yml

derived_data="$app_directory/build/InstallDerivedData"

build_arguments=(
  -project AIQuotaWidget.xcodeproj
  -scheme AIQuotaInstall
  -configuration Debug
  -derivedDataPath "$derived_data"
  CODE_SIGNING_ALLOWED=NO
  CODE_SIGNING_REQUIRED=NO
  build
)

xcodebuild "${build_arguments[@]}"

app_source="$derived_data/Build/Products/Debug/AIQuota.app"
if [[ ! -d "$app_source" ]]; then
  print -u2 "Build succeeded but AIQuota.app was not found at $app_source"
  exit 1
fi

config_directory="$HOME/.config/ai-quota-widget"
config_file="$config_directory/config.json"
mkdir -p "$config_directory" "$HOME/Applications"
if [[ ! -e "$config_file" ]]; then
  cp Resources/DefaultConfig.json "$config_file"
fi

app_destination="$HOME/Applications/AI Quota.app"
ditto "$app_source" "$app_destination"
test_bundle="$app_destination/Contents/PlugIns/AIQuotaTests.xctest"
if [[ -d "$test_bundle" ]]; then
  rm -R "$test_bundle"
fi
signing_identity="$(security find-identity -v -p codesigning | awk '/Apple Development:/ { print $2; exit }')"
if [[ -n "$signing_identity" ]]; then
  widget_extension="$app_destination/Contents/PlugIns/AIQuotaWidgetExtension.appex"
  codesign --force --sign "$signing_identity" \
    --entitlements Resources/AIQuotaWidgetExtension.entitlements \
    "$widget_extension"
  codesign --force --sign "$signing_identity" \
    --entitlements Resources/AIQuota.entitlements \
    "$app_destination"
fi
pluginkit -a "$app_destination/Contents/PlugIns/AIQuotaWidgetExtension.appex" >/dev/null 2>&1 || true
open "$app_destination"

print "Installed and launched: $app_destination"
print "Config: $config_file"
