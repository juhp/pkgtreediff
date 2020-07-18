stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts build
	@echo
	stack --resolver lts-15 build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 build
#	@echo
#	stack --resolver lts-12 build
#	@echo
#	stack --resolver lts-11 build
#	@echo
#	stack --resolver lts-10 build
#	@echo
#	stack --resolver lts-9 build
#	@echo
#	stack --resolver lts-8 build
