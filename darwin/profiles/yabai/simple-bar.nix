{ ... }:

{
  homebrew.casks = [ "ubersicht" ];

  system.activationScripts.postUserActivation.text = ''
    widgetDir="$HOME/Library/Application Support/Übersicht/widgets"
    [[ -f "$widgetDir/GettingStarted.jsx" ]] && rm "$widgetDir/GettingStarted.jsx"
    [[ -f "$widgetDir/logo.png" ]] && rm "$widgetDir/logo.png"
    [[ ! -d "$widgetDir/simple-bar-lite" ]] && git clone https://github.com/Jean-Tinland/simple-bar-lite "$widgetDir/simple-bar-lite"
  '';
}
