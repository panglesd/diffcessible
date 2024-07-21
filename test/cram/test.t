This is a cram test for the new executable.

# Run the executable
  $ dummy_terminal example.diff h q
  Operation 1 of 16, 8 hunks
  10 additions, 9 removals
  Rename with modifications a/document.txt to b/document.txt
  @@ -1,5 +1,5 @@
   1    - The quick brown fox jumps dog. the lazy dog. 
      1 + The fast brown fox leaps over the sleeping dog.  
   2  2   She sells seashells by the seashore. 
   3    -  mainly rain in Spain stays mainly on the plain. 
      3 + The rain in France falls mainly  on the plain. 
   4  4   To be or not to be, that is the question. 
   5    - All gold. glitters is not gold. 
      5 + Not all that glitters is gold.  
  @@ -7,3 +7,3 @@
   7  7   A stitch in time saves nine. 
   8    - An apple a day keeps the doctor away. 
      8 + An apple each day keeps the doctor away. 
   9  9   Actions speak louder than words. 
  @@ -11,2 +11,2 @@
  11    - The early bird catches the worm. 
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.
  $ dummy_terminal example.diff n
  Operation 2 of 16, 3 hunks
  3 additions, 3 removals
  Rename with modifications a/sample.txt to b/sample.txt
  @@ -1,2 +1,2 @@
   1    - foo bar baz 
      1 + foo bat bak 
  @@ -4,2 +4,1 @@
   4    - The quick brown fox jumps over the lazy dog. 
      4 + The quick black cat jumps over the lazy dog. 
  @@ -7,2 +6,2 @@
   7    - She the sea shells by the sea shore. 
      6 + He sells sea shells by the sea floor. 
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.
  $ dummy_terminal example.diff n n
  Operation 3 of 16, 1 hunk
  1 addition, 1 removal
  Modification of bin/dune
  @@ -1,4 +1,4 @@
   1  1   (executable 
   2  2    (public_name diffcessible) 
   3  3    (name main) 
   4    -  (libraries diffcessible cmdliner)) 
      4 +  (libraries diffcessible cmdliner patch)) 
  
  
  
  
  
  
  
  
  
  
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
    1 - Hi everyone!                                                            1 + Hello World!
                                                                                2 + This is the diffcessible project.
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal more-examples.diff n t
  Operation 2 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -3,1 @@                                                                  @@ +5,2 @@
    1 - This file starts at line 3.                                             1 + This file starts at line 5.
                                                                                2 + This is the second test case in this file.
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal example.diff n n t
  Operation 3 of 16, 1 hunk
  1 addition, 1 removal
  Modification of bin/dune
  @@ -1,4 @@                                                                  @@ +1,4 @@
    1   (executable                                                             1   (executable
    2    (public_name diffcessible)                                             2    (public_name diffcessible)
    3    (name main)                                                            3    (name main)
    4 -  (libraries diffcessible cmdliner))                                     4 +  (libraries diffcessible cmdliner patch))
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal example.diff n n n t
  Operation 4 of 16, 1 hunk
  3 additions, 1 removal
  Modification of bin/main.ml
  @@ -1,7 @@                                                                  @@ +1,9 @@
    1   open Diffcessible                                                       1   open Diffcessible
    2                                                                           2   
    3   let main () =                                                           3   let main () =
    4 -   Interactive_viewer.start ()                                           4 +   let s = In_channel.input_all In_channel.stdin in
                                                                                5 +   let patch = Patch.to_diffs s in
                                                                                6 +   Interactive_viewer.start patch
    5                                                                           7   
    6   open Cmdliner                                                           8   open Cmdliner
    7                                                                           9   
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.


  $ dummy_terminal example.diff n n n n t
  Operation 5 of 16, 1 hunk
  1 addition, 1 removal
  Modification of lib/dune
  @@ -1,3 @@                                                                  @@ +1,3 @@
    1   (library                                                                1   (library
    2    (name diffcessible)                                                    2    (name diffcessible)
    3 -  (libraries notty nottui lwd))                                          3 +  (libraries notty nottui lwd patch))
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.


  $ dummy_terminal example.diff n n n n n t
  Operation 6 of 16, 1 hunk
  6 additions, 20 removals
  Modification of lib/interactive_viewer.ml
  @@ -1,39 @@                                                                 @@ +1,25 @@
    1   open Nottui                                                             1   open Nottui
    2   module W = Nottui_widgets                                               2   module W = Nottui_widgets
    3 - open Lwd_infix                                                          3 + (* open Lwd_infix *)
    4 -                                                                             
    5 - type patch = unit                                                           
    6                                                                           4   
    7   let pure_str s = Lwd.pure (W.string s)                                  5   let pure_str s = Lwd.pure (W.string s)
    8 -                                                                             
    9 - let string_of_counter c =                                                   
   10 -   let$ c = c in                                                             
   11 -   W.string (string_of_int c)                                                
   12 -                                                                             
   13   let quit = Lwd.var false                                                6   let quit = Lwd.var false
   14 - let counter = Lwd.var 0                                                 7 + let string_of_operation = Format.asprintf "%a" (Patch.pp_operation ~
   15 - let counter_d = Lwd.get counter                                             
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal example.diff n n n n n n n t
  Operation 8 of 16, 1 hunk
  1 addition, 1 removal
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -1,1 @@                                                                  @@ +1,1 @@
    1 - This is the original content.                                           1 + This is the modified content.
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal example.diff n n n n n n n n n t
  Operation 10 of 16, 1 hunk
  2 additions, 1 removal
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -1,1 @@                                                                  @@ +1,2 @@
    1 - This is the original content.                                           1 + Here is some additional line.
                                                                                2 + Deleted line 1 and added this.
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal example.diff n n n n n n n n n n n n t
  Operation 13 of 16, 0 hunks
  0 additions, 0 removals
  Rename dir1/file.txt to dir2/new_file.txt
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.

  $ dummy_terminal example.diff n n n n n n n n n n n n n n n t
  Operation 16 of 16, 1 hunk
  1 addition, 0 removals
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -0,0 @@                                                                  @@ +1,1 @@
                                                                                1 + new text
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.


  $ dummy_terminal example.diff n n n n n n n n n n n n n n n n t
  Operation 16 of 16, 1 hunk
  1 addition, 0 removals
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -0,0 @@                                                                  @@ +1,1 @@
                                                                                1 + new text
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Type 'h' to go to the help panel, 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation. Press 't' to toggle view mode.







