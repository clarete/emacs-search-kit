# Emacs Search Kit

Just a small toolkit that helps emacs users to find files by either
their names or patterns in their contents.

## How to use it

First of all, clone our repo to somewhere

```bash
$ git clone https://github.com/clarete/emacs-search-kit.git esk
```

Now, edit your `.emacs` and add the following lines

```lisp
(add-to-list 'load-path "/path/to/your/esk/clone")
(require 'esk)
```

Finally, you're ready to use it. Just do `M-x esk-find-file` or
`M-x esk-find-in-project`.

It's also a good idea to bind these functions to a keyboard
shortcut. The following commands will do this job:

```lisp
(global-set-key "\M-s" 'esk-find-file)
(global-set-key "\M-\S-s" 'esk-find-in-project)
```

## License

```
Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>
Copyright (C) 2012  Suneel Chakravorty <suneel0101@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
```
