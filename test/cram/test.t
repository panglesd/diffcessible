This is a cram test for the new executable.

# Run the executable
  $ touch example.diff
  $ echo "diff --git a/dir1/file.txt b/dir2/file.txt\nindex dfd0d1e..ce59064 100644\n--- a/dir1/file.txt\n+++ b/dir2/file.txt\n@@ -1 +1 @@\n-This is the original content.\n+This is the modified content." > example.diff
  $ dummy_terminal example.diff q
  Operation 1 of 1, 1 hunk
  1 addition, 1 removal
  Rename with modifications dir1/file.txt to dir2/file.txt
  @@ -0,1 +0,1 @@
  - This is the original content.
  + This is the modified content.
  
  
  
  
  
  
  
  
  
  Type 'q' to quit, 'n' to go to the next operation, 'p' to go to the previous operation
