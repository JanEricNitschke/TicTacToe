(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 0
    local.set 1
    local.get 1
    i32.load8_u
    i32.eqz
    if (result i32)  ;; label = @1
      local.get 1
      global.get 0
      i32.const 1
      i32.add
      i32.store8
      global.get 0
      i32.const 1
      i32.xor
      global.set 0
      i32.const 1
    else
      i32.const 0
    end)
  (memory (;0;) 1)
  (global (;0;) (mut i32) (i32.const 0))
  (export "memory" (memory 0))
  (export "play" (func 0)))
