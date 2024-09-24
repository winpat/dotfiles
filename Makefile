.PHONY: help bin mail terminfo dotfiles

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

bin: ## Move scripts and tooling in place
	mkdir -p ~/bin
	for file in $(shell find $(CURDIR)/bin -type f -not -name ".*.swp"); do \
		f=$$(basename $$file); \
		ln -sf $$file ~/bin/$$f; \
	done

terminfo: ## Create .terminfo for 24bit colors
	tic -x -o ~/.terminfo terminfo/xterm-24bit.terminfo
	sudo tic -x -o /root/.terminfo terminfo/xterm-24bit.terminfo

dotfiles: ## Link dotfiles
	dotm
