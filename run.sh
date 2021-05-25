#!/bin/bash

# Copyright 2019 The CodeWorld Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source base.sh

#run . rm -rf data/base/* data/codeworld/build/*
#run . rm -rf data/*/build
#rm -rf $(for fn in $(find data/base -atime +20 -regex .*\\.js$ ); do dirname $fn; done)

if [ "x$1" = "x-c" ]; then
	run . rm -rf data/base/* data/codeworld/build/*
	echo "Cleaned and exited"
	exit 0
fi

fuser -k -n tcp 8080
fuser -k -n tcp 9160
sleep 2

# Run migration of project directory structure for codeworld-server.
#mkdir -p data/codeworld/projects
#mkdir -p data/haskell/projects
#mkdir -p data/blocklyXML/projects
#run . codeworld-server-migrate

#mkdir -p log
mkdir -p log data/codeworld/build

echo "Running LSU version"
#codeworld-game-server +RTS -T &
run .  codeworld-server -p 8080

