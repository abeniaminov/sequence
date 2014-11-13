@echo off
set PA=
set PA=%PA% ebin
set PA=%PA% include
werl +pc unicode -sname foo -pa %PA%  -config etc/sequence.config  -args_file etc/vm.args
