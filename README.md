# grunt.el

[Grunt][] is a renowned JavaScript task runner. This package provides
a way to invoke grunt tasks without needing a separate shell or buffer
management per task or project.

## installation

Use MELPA: `M-x package-install <RET> grunt <RET>`

## usage

Figure out what you want to bind `grunt-exec` to, or just invoke it
via `M-x` while your current buffer is in a repo with a Gruntfile
somewhere.

    ;; currently experimenting with "C-M-g"
    (global-set-key (kbd "C-M-g") 'grunt-exec)

It'll try to find your Gruntfile and suggest tasks to run if it can
find them:

![screenshot.png](screenshot.png)

## development

There's clearly tons of room for improvement - in particular, it'd be
great to dip into the AST from `js2-mode` and parse out all of the
valid tasks instead of hackily doing a `(string-match)` for
`registerTask`.

To run tests, use [Cask][]:

    $ cask install
    $ cask exec ert-runner

[Grunt]: http://gruntjs.com/
[Cask]: http://cask.github.io/
