# Contributing

Contributions are welcome. If you discover bugs or issues, or have ideas for
improvements or new features, please file a report on the issue tracker for this
repository. Follow the guidelines below to make sure everything goes smoothly.

## Issue reporting

- Check that the issue has not already been reported
- Check that the issue has not already been fixed in the latest code
- Open an issue with a clear title
- Write as grammatically correct as you can in the description.

## Pull requests

- Perform all changes on a topic branch for easier merging
- Follow the coding conventions already in use
- Verify Emacs Lisp code with `checkdoc`
- Add unit tests whenever possible
- Open a [merge request](https://gitlab.petton.fr/nico/Indium/merge_requests)
  relating to a single issue.

## Coding Conventions

### Naming

- Use an `indium-` prefix for all public names.
- Use an `indium--` prefix for all internal names.

### Docstrings

Write meaningful docstrings for all functions and vars.

- Document all functions and variables as directed by `checkdoc`.
- Consider using [Flycheck](https://github.com/flycheck/flycheck) to automate
  `checkdoc` while you're editing.

### Unit tests

- Try to add unit tests for all contributions
- Bug fixes should ideally come with regression tests.

### Common Lisp functions

Use `cl-lib` instead of `cl`. The `cl` library pollutes the global namespace and
its usage is therefore discouraged.

- Use `cl-lib`, which adds prefixes to all cl function names
- Use [noflet](https://github.com/nicferrier/emacs-noflet) instead of `flet`
  when you need to dynamically rebind functions.
  
### seq.el and map.el

Use `seq-` and `map-` functions for manipulating collections whenever possible.
