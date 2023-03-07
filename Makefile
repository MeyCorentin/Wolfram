#
## EPITECH PROJECT, 2021
## Makefile
## File description:
## It's my Makefile
##


NAME = wolfram

${NAME}:
	stack build --copy-bins

all : ${NAME}

clean :
	stack clean
	rm -rf wolfram
	@echo 'Temporary files deleted'

fclean : clean
	stack purge
	@echo 'Executable deleted'

re : fclean all

.PHONY: re fclean clean all