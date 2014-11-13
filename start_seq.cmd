@echo off
set PA=
set PA=%PA% ebin
set PA=%PA% include
werl +pc unicode -sname seq -pa %PA%  -config etc/sequence.config  -args_file etc/vm_seq.args
