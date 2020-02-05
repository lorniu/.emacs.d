help:
	@echo - make update
	@echo - make pack/compile
	@echo - make clean

update:
	git submodule foreach "git checkout -q master && git pull --rebase && echo"
	git pull --rebase

pack:
	@echo Packing current emacs configs...
	sh ./bin/emacs-dot-pack-myself-for-backup.sh

compile:
	emacs --batch --eval '(byte-recompile-directory "~/.emacs.d/extra" 0)'

clean:
	@echo Ready to clean...
	rm -rf .cache
	rm -rf auto-save-list
	rm -rf multisession
	rm -rf eln-cache
	rm -f  network-security.data
	find core/ -name "*.elc" -exec rm {} \;
	find extra/ -name "*.elc" -exec rm {} \;
	@echo Clean done!
