This is a cram test for the new executable.

# Run the executable
  $ dummy_terminal example.diff n n
  Operation 2 of 14, 1 hunk
  3 additions, 1 removal
  Modification of bin/main.ml
  @@ -0,7 +0,9 @@
    open Diffcessible
    
    let main () =
  -   Interactive_viewer.start ()
  +   let s = In_channel.input_all In_channel.stdin in
  +   let patch = Patch.to_diffs s in
  +   Interactive_viewer.start patch
    
    open Cmdliner
    
  
  Type 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
