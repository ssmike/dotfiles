add-auto-load-safe-path /usr/lib64/go/src/pkg/runtime/runtime-gdb.py
set confirm off
set height 0
set width 0

define argv
  show args
end
document argv
Print program arguments.
end

define btt
  thread apply $arg0 bt
end
document btt
Print stack for specific thread
end
