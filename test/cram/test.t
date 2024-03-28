This is a cram test for the new executable.

# Run the executable
  $ dummy_terminal example.diff h q
  Operation 1 of 14, 1 hunk
  1 addition, 1 removal
  Modification of bin/dune
  @@ -0,4 +0,4 @@
    (executable
     (public_name diffcessible)
     (name main)
  -  (libraries diffcessible cmdliner))
  +  (libraries diffcessible cmdliner patch))
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
  $ dummy_terminal example.diff n
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
    
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
  $ dummy_terminal example.diff n n
  Operation 3 of 14, 1 hunk
  1 addition, 1 removal
  Modification of lib/dune
  @@ -0,3 +0,3 @@
    (library
     (name diffcessible)
  -  (libraries notty nottui lwd))
  +  (libraries notty nottui lwd patch))
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
  $ dummy_terminal example.diff h 
  Help Panel:
  
  h:   Open the help panel
  q:   Quit the diffcessible viewer
  n:   Move to the next operation, if present
  p:   Move to the previous operation, if present
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'q' to exit the help panel
