This is a cram test for the new executable.

# Run the executable
  $ dummy_terminal example.diff h q
  Operation 1 of 14, 1 hunk
  1 addition, 1 removal
  Modification of bin/dune
  @@ -1,4 +1,4 @@
   1  1   (executable
   2  2    (public_name diffcessible)
   3  3    (name main)
   4    -  (libraries diffcessible cmdliner))
      4 +  (libraries diffcessible cmdliner patch))
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.
  $ dummy_terminal example.diff n
  Operation 2 of 14, 1 hunk
  3 additions, 1 removal
  Modification of bin/main.ml
  @@ -1,7 +1,9 @@
   1  1   open Diffcessible
   2  2   
   3  3   let main () =
   4    -   Interactive_viewer.start ()
      4 +   let s = In_channel.input_all In_channel.stdin in
      5 +   let patch = Patch.to_diffs s in
      6 +   Interactive_viewer.start patch
   5  7   
   6  8   open Cmdliner
   7  9   
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.
  $ dummy_terminal example.diff n n
  Operation 3 of 14, 1 hunk
  1 addition, 1 removal
  Modification of lib/dune
  @@ -1,3 +1,3 @@
   1  1   (library
   2  2    (name diffcessible)
   3    -  (libraries notty nottui lwd))
      3 +  (libraries notty nottui lwd patch))
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.
  $ dummy_terminal example.diff h 
  Help Panel:
  
  h:   Open the help panel
  q:   Quit the diffcessible viewer
  n:   Move to the next operation, if present
  p:   Move to the previous operation, if present
  t:   Toggle view mode
  l:   Toggle line numbers
  
  
  
  
  
  
  
  
  
  
  
  Type 'q' to exit the help panel
  $ dummy_terminal more-examples.diff h q
  Operation 1 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -2,1 +2,2 @@
   2    - Hi everyone!
      2 + Hello World!
      3 + This is the diffcessible project.
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.
  $ dummy_terminal more-examples.diff n
  Operation 2 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -3,1 +5,2 @@
   3    - This file starts at line 3.
      5 + This file starts at line 5.
      6 + This is the second test case in this file.
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.






# Testing for side by side view 
  $ dummy_terminal more-examples.diff t 
  Operation 1 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -2,1 @@                                                                  @@ +2,2 @@
    2 - Hi everyone!                                                            2 + Hello World!
                                                                                3 + This is the diffcessible project.
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal more-examples.diff n t
  Operation 2 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -3,1 @@                                                                  @@ +5,2 @@
    3 - This file starts at line 3.                                             5 + This file starts at line 5.
                                                                                6 + This is the second test case in this file.
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal more-examples.diff n n t
  Operation 2 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -3,1 @@                                                                  @@ +5,2 @@
    3 - This file starts at line 3.                                             5 + This file starts at line 5.
                                                                                6 + This is the second test case in this file.
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

