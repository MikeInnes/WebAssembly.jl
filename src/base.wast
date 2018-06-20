
  ;; Changes to the original include:
  ;;  * Subtracting 1 from the index passed to get.
  ;;  * Array length needs to return an int64
  ;;  * Array pointers are u32 here but u64 in Julia



  (type (;0;) (func))
  (type (;1;) (func (param i32 i32) (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func (param i32 i32 i32 i32)))
  (type (;4;) (func (param i32)))
  (type (;5;) (func (param i32 i32)))
  (import "env" "abort" (func $~lib/env/abort (type 3)))
  (func $~lib/array/Array<u32>#get:length (param i32) (result i32)
    get_local 0
    i32.load offset=4
    return)
  (func $~lib/array/Array<i64>#__get (param $p0 i32) (param $p1 i32) (result i64)
    (local $l0 i32)
    get_local $p0
    i32.load
    set_local $l0
    get_local $p1
    get_local $l0
    i32.load
    i32.const 3
    i32.shr_u
    i32.lt_u
    if $I0 (result i64)
      block $B1 (result i64)
        get_local $l0
        get_local $p1
        i32.const 3
        i32.shl
        i32.add
        i64.load offset=8
        br $B1
      end
    else
      unreachable
    end
    return)
  (func $~lib/array/Array<u32>#__get (param i32 i64) (result i32)
    (local i32) (local i32)
    (get_local 1)
    (i32.wrap/i64)
    (tee_local 3)
    (i32.const 1)
    (i32.sub)
    (set_local 3)
    get_local 0
    i32.load
    set_local 2
    get_local 3
    get_local 2
    i32.load
    i32.const 2
    i32.shr_u
    i32.lt_u
    if (result i32)  ;; label = @1
      block (result i32)  ;; label = @2
        get_local 2
        get_local 3
        i32.const 2
        i32.shl
        i32.add
        i32.load offset=8
        br 0 (;@2;)
      end
    else
      unreachable
    end
    return)
  (func $~lib/allocator/buddy/update_max_ptr (type 2) (param i32) (result i32)
    (local i32 i32)
    get_local 0
    get_global 17
    i32.gt_u
    if  ;; label = @1
      current_memory
      set_local 1
      get_local 0
      i32.const 65535
      i32.add
      i32.const 65535
      i32.const -1
      i32.xor
      i32.and
      i32.const 16
      i32.shr_u
      set_local 2
      get_local 2
      get_local 1
      i32.gt_u
      i32.eqz
      if  ;; label = @2
        i32.const 0
        i32.const 8
        i32.const 181
        i32.const 4
        call $~lib/env/abort
        unreachable
      end
      get_local 2
      get_local 1
      i32.sub
      grow_memory
      i32.const 0
      i32.lt_s
      if  ;; label = @2
        i32.const 0
        return
      end
      get_local 2
      i32.const 16
      i32.shl
      set_global 17
    end
    i32.const 1
    return)
  (func $~lib/allocator/buddy/buckets$get (type 2) (param i32) (result i32)
    get_local 0
    i32.const 27
    i32.lt_u
    i32.eqz
    if  ;; label = @1
      i32.const 0
      i32.const 8
      i32.const 101
      i32.const 2
      call $~lib/env/abort
      unreachable
    end
    get_global 10
    get_local 0
    i32.const 8
    i32.mul
    i32.add
    return)
  (func $~lib/allocator/buddy/list_init (type 4) (param i32)
    get_local 0
    get_local 0
    i32.store
    get_local 0
    get_local 0
    i32.store offset=4)
  (func $~lib/allocator/buddy/list_push (type 5) (param i32 i32)
    (local i32)
    get_local 0
    i32.load
    set_local 2
    get_local 1
    get_local 2
    i32.store
    get_local 1
    get_local 0
    i32.store offset=4
    get_local 2
    get_local 1
    i32.store offset=4
    get_local 0
    get_local 1
    i32.store)
  (func $~lib/allocator/buddy/bucket_for_request (type 2) (param i32) (result i32)
    (local i32 i32)
    i32.const 27
    i32.const 1
    i32.sub
    set_local 1
    i32.const 16
    set_local 2
    block  ;; label = @1
      loop  ;; label = @2
        get_local 2
        get_local 0
        i32.lt_u
        if  ;; label = @3
          block  ;; label = @4
            get_local 1
            i32.const 1
            i32.sub
            set_local 1
            get_local 2
            i32.const 2
            i32.mul
            set_local 2
          end
          br 1 (;@2;)
        end
      end
    end
    get_local 1
    return)
  (func $~lib/allocator/buddy/node_for_ptr (type 1) (param i32 i32) (result i32)
    get_local 0
    get_global 16
    i32.sub
    i32.const 30
    get_local 1
    i32.sub
    i32.shr_u
    i32.const 1
    get_local 1
    i32.shl
    i32.add
    i32.const 1
    i32.sub
    return)
  (func $~lib/allocator/buddy/node_is_split$get (type 2) (param i32) (result i32)
    get_local 0
    i32.const 8388608
    i32.lt_u
    i32.eqz
    if  ;; label = @1
      i32.const 0
      i32.const 8
      i32.const 147
      i32.const 2
      call $~lib/env/abort
      unreachable
    end
    get_global 14
    get_local 0
    i32.add
    i32.load8_u
    return)
  (func $~lib/allocator/buddy/parent_is_split (type 2) (param i32) (result i32)
    get_local 0
    i32.const 1
    i32.sub
    i32.const 2
    i32.div_u
    set_local 0
    get_local 0
    i32.const 8
    i32.div_u
    call $~lib/allocator/buddy/node_is_split$get
    get_local 0
    i32.const 8
    i32.rem_u
    i32.shr_u
    i32.const 1
    i32.and
    i32.const 1
    i32.eq
    return)
  (func $~lib/allocator/buddy/list_remove (type 4) (param i32)
    (local i32 i32)
    get_local 0
    i32.load
    set_local 1
    get_local 0
    i32.load offset=4
    set_local 2
    get_local 1
    get_local 2
    i32.store offset=4
    get_local 2
    get_local 1
    i32.store)
  (func $~lib/allocator/buddy/ptr_for_node (type 1) (param i32 i32) (result i32)
    get_global 16
    get_local 0
    i32.const 1
    get_local 1
    i32.shl
    i32.sub
    i32.const 1
    i32.add
    i32.const 30
    get_local 1
    i32.sub
    i32.shl
    i32.add
    return)
  (func $~lib/allocator/buddy/node_is_split$set (type 5) (param i32 i32)
    get_local 0
    i32.const 8388608
    i32.lt_u
    i32.eqz
    if  ;; label = @1
      i32.const 0
      i32.const 8
      i32.const 152
      i32.const 2
      call $~lib/env/abort
      unreachable
    end
    get_global 14
    get_local 0
    i32.add
    get_local 1
    i32.store8)
  (func $~lib/allocator/buddy/flip_parent_is_split (type 4) (param i32)
    (local i32)
    get_local 0
    i32.const 1
    i32.sub
    i32.const 2
    i32.div_u
    set_local 0
    get_local 0
    i32.const 8
    i32.div_u
    set_local 1
    get_local 1
    get_local 1
    call $~lib/allocator/buddy/node_is_split$get
    i32.const 1
    get_local 0
    i32.const 8
    i32.rem_u
    i32.shl
    i32.xor
    call $~lib/allocator/buddy/node_is_split$set)
  (func $~lib/allocator/buddy/lower_bucket_limit (type 2) (param i32) (result i32)
    (local i32 i32)
    block  ;; label = @1
      loop  ;; label = @2
        get_local 0
        get_global 12
        i32.lt_u
        if  ;; label = @3
          block  ;; label = @4
            get_global 16
            get_global 12
            call $~lib/allocator/buddy/node_for_ptr
            set_local 1
            get_local 1
            call $~lib/allocator/buddy/parent_is_split
            i32.eqz
            if  ;; label = @5
              get_global 16
              call $~lib/allocator/buddy/list_remove
              block (result i32)  ;; label = @6
                get_global 12
                i32.const 1
                i32.sub
                set_global 12
                get_global 12
              end
              call $~lib/allocator/buddy/buckets$get
              call $~lib/allocator/buddy/list_init
              get_global 12
              call $~lib/allocator/buddy/buckets$get
              get_global 16
              call $~lib/allocator/buddy/list_push
              br 3 (;@2;)
            end
            get_local 1
            i32.const 1
            i32.add
            get_global 12
            call $~lib/allocator/buddy/ptr_for_node
            set_local 2
            get_local 2
            i32.const 8
            i32.add
            call $~lib/allocator/buddy/update_max_ptr
            i32.eqz
            if  ;; label = @5
              i32.const 0
              return
            end
            get_global 12
            call $~lib/allocator/buddy/buckets$get
            get_local 2
            call $~lib/allocator/buddy/list_push
            block (result i32)  ;; label = @5
              get_global 12
              i32.const 1
              i32.sub
              set_global 12
              get_global 12
            end
            call $~lib/allocator/buddy/buckets$get
            call $~lib/allocator/buddy/list_init
            get_local 1
            i32.const 1
            i32.sub
            i32.const 2
            i32.div_u
            set_local 1
            get_local 1
            i32.const 0
            i32.ne
            if  ;; label = @5
              get_local 1
              call $~lib/allocator/buddy/flip_parent_is_split
            end
          end
          br 1 (;@2;)
        end
      end
    end
    i32.const 1
    return)
  (func $~lib/allocator/buddy/list_pop (type 2) (param i32) (result i32)
    (local i32)
    get_local 0
    i32.load
    set_local 1
    get_local 1
    get_local 0
    i32.eq
    if  ;; label = @1
      i32.const 0
      return
    end
    get_local 1
    call $~lib/allocator/buddy/list_remove
    get_local 1
    return)
  (func $~lib/allocator/buddy/allocate_memory (type 2) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    get_local 0
    i32.const 1073741824
    i32.const 8
    i32.sub
    i32.gt_u
    if  ;; label = @1
      unreachable
    end
    get_global 16
    i32.const 0
    i32.eq
    if  ;; label = @1
      get_global 15
      i32.const 7
      i32.add
      i32.const 7
      i32.const -1
      i32.xor
      i32.and
      set_global 16
      current_memory
      i32.const 16
      i32.shl
      set_global 17
      i32.const 27
      i32.const 1
      i32.sub
      set_global 12
      get_global 16
      i32.const 8
      i32.add
      call $~lib/allocator/buddy/update_max_ptr
      i32.eqz
      if  ;; label = @2
        i32.const 0
        return
      end
      i32.const 27
      i32.const 1
      i32.sub
      call $~lib/allocator/buddy/buckets$get
      call $~lib/allocator/buddy/list_init
      i32.const 27
      i32.const 1
      i32.sub
      call $~lib/allocator/buddy/buckets$get
      get_global 16
      call $~lib/allocator/buddy/list_push
    end
    get_local 0
    i32.const 8
    i32.add
    call $~lib/allocator/buddy/bucket_for_request
    set_local 2
    get_local 2
    set_local 1
    block  ;; label = @1
      loop  ;; label = @2
        get_local 2
        i32.const 1
        i32.add
        i32.const 0
        i32.ne
        if  ;; label = @3
          get_local 2
          call $~lib/allocator/buddy/lower_bucket_limit
          i32.eqz
          if  ;; label = @4
            i32.const 0
            return
          end
          get_local 2
          call $~lib/allocator/buddy/buckets$get
          call $~lib/allocator/buddy/list_pop
          set_local 6
          get_local 6
          i32.eqz
          if  ;; label = @4
            get_local 2
            get_global 12
            i32.ne
            tee_local 7
            if (result i32)  ;; label = @5
              get_local 7
            else
              get_local 2
              i32.const 0
              i32.eq
            end
            if  ;; label = @5
              get_local 2
              i32.const 1
              i32.sub
              set_local 2
              br 3 (;@2;)
            end
            get_local 2
            i32.const 1
            i32.sub
            call $~lib/allocator/buddy/lower_bucket_limit
            i32.eqz
            if  ;; label = @5
              i32.const 0
              return
            end
            get_local 2
            call $~lib/allocator/buddy/buckets$get
            call $~lib/allocator/buddy/list_pop
            set_local 6
          end
          i32.const 1
          i32.const 30
          get_local 2
          i32.sub
          i32.shl
          set_local 3
          get_local 2
          get_local 1
          i32.lt_u
          if (result i32)  ;; label = @4
            get_local 3
            i32.const 2
            i32.div_u
            i32.const 8
            i32.add
          else
            get_local 3
          end
          set_local 4
          get_local 6
          get_local 4
          i32.add
          call $~lib/allocator/buddy/update_max_ptr
          i32.eqz
          if  ;; label = @4
            get_local 2
            call $~lib/allocator/buddy/buckets$get
            get_local 6
            call $~lib/allocator/buddy/list_push
            i32.const 0
            return
          end
          get_local 6
          get_local 2
          call $~lib/allocator/buddy/node_for_ptr
          set_local 5
          get_local 5
          i32.const 0
          i32.ne
          if  ;; label = @4
            get_local 5
            call $~lib/allocator/buddy/flip_parent_is_split
          end
          block  ;; label = @4
            loop  ;; label = @5
              get_local 2
              get_local 1
              i32.lt_u
              if  ;; label = @6
                block  ;; label = @7
                  get_local 5
                  i32.const 2
                  i32.mul
                  i32.const 1
                  i32.add
                  set_local 5
                  get_local 2
                  i32.const 1
                  i32.add
                  set_local 2
                  get_local 5
                  call $~lib/allocator/buddy/flip_parent_is_split
                  get_local 2
                  call $~lib/allocator/buddy/buckets$get
                  get_local 5
                  i32.const 1
                  i32.add
                  get_local 2
                  call $~lib/allocator/buddy/ptr_for_node
                  call $~lib/allocator/buddy/list_push
                end
                br 1 (;@5;)
              end
            end
          end
          get_local 6
          get_local 0
          i32.store
          get_local 6
          i32.const 8
          i32.add
          return
        end
      end
    end
    i32.const 0
    return)
  (func $main/allocate (type 2) (param i32) (result i32)
    get_local 0
    call $~lib/allocator/buddy/allocate_memory
    return)
  (func $~lib/allocator/buddy/free_memory (type 4) (param i32)
    (local i32 i32 i32)
    get_local 0
    i32.eqz
    if  ;; label = @1
      return
    end
    get_local 0
    i32.const 8
    i32.sub
    set_local 0
    get_local 0
    i32.load
    i32.const 8
    i32.add
    call $~lib/allocator/buddy/bucket_for_request
    set_local 1
    get_local 0
    get_local 1
    call $~lib/allocator/buddy/node_for_ptr
    set_local 2
    block  ;; label = @1
      loop  ;; label = @2
        get_local 2
        i32.const 0
        i32.ne
        if  ;; label = @3
          block  ;; label = @4
            get_local 2
            call $~lib/allocator/buddy/flip_parent_is_split
            get_local 2
            call $~lib/allocator/buddy/parent_is_split
            tee_local 3
            if (result i32)  ;; label = @5
              get_local 3
            else
              get_local 1
              get_global 12
              i32.eq
            end
            if  ;; label = @5
              br 4 (;@1;)
            end
            get_local 2
            i32.const 1
            i32.sub
            i32.const 1
            i32.xor
            i32.const 1
            i32.add
            get_local 1
            call $~lib/allocator/buddy/ptr_for_node
            call $~lib/allocator/buddy/list_remove
            get_local 2
            i32.const 1
            i32.sub
            i32.const 2
            i32.div_u
            set_local 2
            get_local 1
            i32.const 1
            i32.sub
            set_local 1
          end
          br 1 (;@2;)
        end
      end
    end
    get_local 1
    call $~lib/allocator/buddy/buckets$get
    get_local 2
    get_local 1
    call $~lib/allocator/buddy/ptr_for_node
    call $~lib/allocator/buddy/list_push)
  (func $main/deallocate (type 4) (param i32)
    get_local 0
    call $~lib/allocator/buddy/free_memory)
  (func $start (type 0)
    get_global 18
    set_global 10
    get_global 10
    i32.const 27
    i32.const 8
    i32.mul
    i32.add
    set_global 11
    get_global 11
    set_global 14
    get_global 14
    i32.const 8388608
    i32.const 1
    i32.mul
    i32.add
    set_global 15)
  (global (;0;) i32 (i32.const 3))
  (global (;1;) i32 (i32.const 8))
  (global (;2;) i32 (i32.const 7))
  (global (;3;) i32 (i32.const 8))
  (global (;4;) i32 (i32.const 8))
  (global (;5;) i32 (i32.const 4))
  (global (;6;) i32 (i32.const 16))
  (global (;7;) i32 (i32.const 30))
  (global (;8;) i32 (i32.const 1073741824))
  (global (;9;) i32 (i32.const 27))
  (global (;10;) (mut i32) (i32.const 0))
  (global (;11;) (mut i32) (i32.const 0))
  (global (;12;) (mut i32) (i32.const 0))
  (global (;13;) i32 (i32.const 8388608))
  (global (;14;) (mut i32) (i32.const 0))
  (global (;15;) (mut i32) (i32.const 0))
  (global (;16;) (mut i32) (i32.const 0))
  (global (;17;) (mut i32) (i32.const 0))
  (global (;18;) i32 (i32.const 60))
  (export "allocate" (func $main/allocate))
  (export "deallocate" (func $main/deallocate))
  (export "memory" (memory 0))
  (start $start)
  (data (i32.const 8) "\17\00\00\00~\00l\00i\00b\00/\00a\00l\00l\00o\00c\00a\00t\00o\00r\00/\00b\00u\00d\00d\00y\00.\00t\00s\00")

  ;; Helper functions to match the call conventions

  (func $arraylen (param i32) (result i64)
    (get_local 0)
    (call $~lib/array/Array<u32>#get:length)
    (i64.extend_s/i32)
    (return)
  )

  (func $arrayref_i64 (param i32) (param i64) (result i64)
    (get_local 0)
    (get_local 1)
    (i32.wrap/i64)
    (i32.const 1)
    (i32.sub)
    (call $~lib/array/Array<i64>#__get)
    (return)
  )
