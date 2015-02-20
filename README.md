findwalker.el
=============

You can use `find` option like S Expression.
Provides easy way of editing find complex arguments and to display
full command-line to echo area.


## Install:

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

```
(autoload 'findwalker "findwalker" "Edit find command with try and error." t)
```

## Usage:

* Following command open editable buffer.

   M-x findwalker

* You can edit `find` command-line option by s-expression like following.

(or (name "HOGE") (type d)) (type f)
  => find . \( -name HOGE -or -type d \) -type f 

(prune (or (name ".svn") (name ".git"))) (type f) (name "*.el")
  => find . \( \( -name .svn -or -name .git \) -prune -or -true \) -type f -name \*.el

Type C-j testing execute above command and display command output.
Type C-c C-c execute command and switch to that buffer.
Type C-c C-q quit editing.
Type M-n, M-p move history if there were.

* TODO in result buffer

