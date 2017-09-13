NAME=compiling

all: $(NAME).html

p: $(NAME).html
	juice $(NAME).html $(NAME)-inline.html

m.idr:
	sed -n '/~~~~{.haskell}/,/~~~~/p' $(NAME).md |grep -v '~~~~' > m.idr

$(NAME).html:
	pandoc -s --highlight-style pygments $(NAME).md > $(NAME).html

clean:
	rm -f $(NAME).html $(NAME).wp.md $(NAME).wp.html

.PHONY: $(NAME).html all p m.idr clean wp $(NAME).wp.md $(NAME).wp.html
