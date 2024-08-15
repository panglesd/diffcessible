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
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode
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
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode
  $ dummy_terminal example.diff n n
  Operation 3 of 14, 1 hunk
  1 addition, 1 removal
  Modification of lib/dune
  @@ -1,3 +1,3 @@
   1  1   (library 
   2  2    (name diffcessible) 
   3    -  (libraries notty nottui lwd)) 
      3 +  (libraries notty nottui lwd patch)) 
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode
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
   1    - Hi everyone! 
      1 + This is the diffcessible project. 
      2 + Hello World! 
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode
  $ dummy_terminal more-examples.diff n
  Operation 2 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -3,1 +5,2 @@
   1    - This file starts at line 3. 
      1 + This is the second test case in this file. 
      2 + This file starts at line 5. 
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode






# Testing for side by side view 
  $ dummy_terminal more-examples.diff t 
  Operation 1 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -2,1 @@                                                                  @@ +2,2 @@
    1 Hi everyone!                                                              1 Hello World!
                                                                                2 This is the diffcessible project.
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode

  $ dummy_terminal more-examples.diff n t
  Operation 2 of 2, 1 hunk
  2 additions, 1 removal
  Modification of file.txt
  @@ -3,1 @@                                                                  @@ +5,2 @@
    1 This file starts at line 3.                                               1 This file starts at line 5.
                                                                                2 This is the second test case in this file.
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode

  $ dummy_terminal example.diff n n t
  Operation 3 of 14, 1 hunk
  1 addition, 1 removal
  Modification of lib/dune
  @@ -1,3 @@                                                                  @@ +1,3 @@
    1   (library                                                                1   (library
    2    (name diffcessible)                                                    2    (name diffcessible)
    3  (libraries notty nottui lwd))                                            3  (libraries notty nottui lwd patch))
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode

  $ dummy_terminal example.diff n n n t
  Operation 4 of 14, 1 hunk
  6 additions, 20 removals
  Modification of lib/interactive_viewer.ml
  @@ -1,39 @@                                                                 @@ +1,25 @@
    1   open Nottui                                                             1   open Nottui
    2   module W = Nottui_widgets                                               2   module W = Nottui_widgets
    3 open Lwd_infix                                                            3 (* open Lwd_infix *)
    4                                                                               
    5 type patch = unit                                                             
    6                                                                           4   
    7   let pure_str s = Lwd.pure (W.string s)                                  5   let pure_str s = Lwd.pure (W.string s)
    8                                                                               
    9 let string_of_counter c =                                                     
   10   let$ c = c in                                                               
   11   W.string (string_of_int c)                                                  
   12                                                                               
   13   let quit = Lwd.var false                                                6   let quit = Lwd.var false
   14 let counter = Lwd.var 0                                                   7 let string_of_operation = Format.asprintf "%a" (Patch.pp_operation ~gi
   15 let counter_d = Lwd.get counter                                               
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode


  $ dummy_terminal example.diff n n n n t
  Operation 5 of 14, 1 hunk
  1 addition, 3 removals
  Modification of lib/interactive_viewer.mli
  @@ -1,5 @@                                                                  @@ +1,3 @@
    1   (** Render and navigate through a diff. *)                              1   (** Render and navigate through a diff. *)
    2                                                                           2   
    3 type patch = unit                                                         3 val start : Patch.t list -> unit
    4                                                                               
    5 val start : patch -> unit                                                     
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode


  $ dummy_terminal example.diff n n n n n t
  Operation 6 of 14, 1 hunk
  1 addition, 1 removal
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -1,1 @@                                                                  @@ +1,1 @@
    1 This is the original content.                                             1 This is the modified content.
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode

  $ dummy_terminal example.diff n n n n n n n t
  Operation 8 of 14, 1 hunk
  2 additions, 1 removal
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -1,1 @@                                                                  @@ +1,2 @@
    1 This is the original content.                                             1 Here is some additional line.
                                                                                2 Deleted line 1 and added this.
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode

  $ dummy_terminal example.diff n n n n n n n n n t
  Operation 10 of 14, 1 hunk
  0 additions, 1 removal
  Deletion of dir1/file.txt
  @@ -1,1 @@                                                                  @@ +1,0 @@
    1 some text                                                                     
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode

  $ dummy_terminal example.diff n n n n n n n n n n n n t
  Operation 13 of 14, 1 hunk
  2 additions, 0 removals
  Creation of dir2/sample.txt
  @@ -1,0 @@                                                                  @@ +1,2 @@
                                                                                1 some text
                                                                                2 lorem ipsum
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode

  $ dummy_terminal example.diff n n n n n n n n n n n n n n n t
  Operation 14 of 14, 1 hunk
  1 addition, 0 removals
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -1,0 @@                                                                  @@ +1,1 @@
                                                                                1 new text
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode


  $ dummy_terminal example.diff n n n n n n n n n n n n n n n n t
  Operation 14 of 14, 1 hunk
  1 addition, 0 removals
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -1,0 @@                                                                  @@ +1,1 @@
                                                                                1 new text
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  [h]elp [q]uit [n/p]avigate [t]oggle view [r]ender mode





