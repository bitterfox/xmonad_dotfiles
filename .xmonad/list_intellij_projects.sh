#!/bin/bash

# list_intellij_projects.sh 2020.2 30

version=$1
lines=$(($2 + 2))

echo $1 $2 $version $lines >> /tmp/debug.idea

grep "-A$lines" recentPaths "$HOME/.config/JetBrains/IntelliJIdea${version}/options/recentProjects.xml" | grep 'option value' | sed -r 's/.*\"([^\"]*)\".*/\1/' | sed -r 's#\$USER_HOME\$#'"$HOME"'#' >> /tmp/debug.idea
grep "-A$lines" recentPaths "$HOME/.config/JetBrains/IntelliJIdea${version}/options/recentProjects.xml" | grep 'option value' | sed -r 's/.*\"([^\"]*)\".*/\1/' | sed -r 's#\$USER_HOME\$#'"$HOME"'#'
