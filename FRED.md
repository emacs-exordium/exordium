My local copy has a subdir called "myemacs" that has the before/after .el files that I use.
When cloning from github into a .emacs.d simply do:

for i in myemacs/*
do
  ln -s $i .
done
