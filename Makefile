all:
	find . -type f -name "*.el" | xargs emacs -batch -f batch-byte-compile