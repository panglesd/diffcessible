This is a cram test for the new executable.

# Run the executable
  $ dummy_terminal example.diff h q
  Operation 1 of 14, 1 hunk
  1 addition, 1 removal
  Modification of bin/dune
  @@ -1,4 +1,4 @@
  1 1   (executable
  2 2    (public_name diffcessible)
  3 3    (name main)
  4   -  (libraries diffcessible cmdliner))
    4 +  (libraries diffcessible cmdliner patch))
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
  $ dummy_terminal example.diff n
  Operation 2 of 14, 1 hunk
  3 additions, 1 removal
  Modification of bin/main.ml
  @@ -1,7 +1,9 @@
  1 1   open Diffcessible
  2 2   
  3 3   let main () =
  4   -   Interactive_viewer.start ()
    4 +   let s = In_channel.input_all In_channel.stdin in
    5 +   let patch = Patch.to_diffs s in
    6 +   Interactive_viewer.start patch
  5 7   
  6 8   open Cmdliner
  7 9   
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
  $ dummy_terminal example.diff n n
  Operation 3 of 14, 1 hunk
  1 addition, 1 removal
  Modification of lib/dune
  @@ -1,3 +1,3 @@
  1 1   (library
  2 2    (name diffcessible)
  3   -  (libraries notty nottui lwd))
    3 +  (libraries notty nottui lwd patch))
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
  $ dummy_terminal example.diff h 
  Help Panel:
  
  h:   Open the help panel
  q:   Quit the diffcessible viewer
  n:   Move to the next operation, if present
  p:   Move to the previous operation, if present
  g:   Scroll back to the top of the displayed operation.
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'q' to exit the help panel
