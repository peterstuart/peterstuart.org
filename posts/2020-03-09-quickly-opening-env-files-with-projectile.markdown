---
title: Quickly Opening .env Files with Projectile
author: Peter Stuart
---

I use [Projectile][projectile] to manage projects in [Emacs][emacs], and `.env` files to manage environment variables for those projects. I edit the `.env` files often enough that I'd like to be able to open them quickly, but they aren't accessible using `projectile-find-file` because they are not tracked by `git`. I wrote this function to quickly open the `.env` file in the project root, or create one if it doesn't exist:

```commonlisp
(defun projectile-env (&optional suffix)
  "Open .env file in the project root."
  (interactive)
  (let ((env-file-path
         (expand-file-name
          (concat ".env" suffix)
          (projectile-project-root))))
    (find-file env-file-path)))
```

It supports an optional `suffix` argument, which I use to open `.env` files with suffixes, like `.env.local`:

```commonlisp
(defun projectile-env-local ()
  "Open .env.local file in the project root."
  (interactive)
  (projectile-env ".local"))
```

I bind all the variations of `projectile-env` with the prefix `C-c C-p e`, followed by a single key (usually the first letter of the suffix to the file name):

| File           | Key Binding   |
| ----           | -----------   |
| `.env`         | `C-c C-p e e` |
| `.env.local`   | `C-c C-p e l` |
| `.env.test`    | `C-c C-p e t` |
| `.env.example` | `C-c C-p e x` |

[projectile]: https://github.com/bbatsov/projectile
[emacs]: https://www.gnu.org/software/emacs
