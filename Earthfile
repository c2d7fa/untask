FROM ubuntu:xenial

RUN apt-get update -y && apt-get install -y software-properties-common
RUN add-apt-repository ppa:plt/racket && apt-get update -y
RUN apt-get install -y racket

RUN apt-get install -y curl
RUN curl -L "https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage" --output /usr/local/bin/appimagetool
RUN chmod +x /usr/local/bin/appimagetool

build:
  COPY . untask
  WORKDIR untask
  RUN raco pkg install --auto --user
  RUN raco exe -o untask main.rkt
  RUN raco distribute AppDir/ untask
  RUN echo '#!/usr/bin/env bash'\\n'exec "$(dirname "$0")/bin/untask" "$@"' > AppDir/AppRun
  RUN chmod +x AppDir/AppRun
  RUN touch AppDir/untask.svg
  RUN echo '[Desktop Entry]'\\n'Name=untask'\\n'Icon=untask'\\n'Type=Application'\\n'Categories=Utility' > AppDir/untask.desktop
  RUN appimagetool --appimage-extract-and-run AppDir untask.AppImage
  SAVE ARTIFACT untask.AppImage AS LOCAL untask.AppImage

