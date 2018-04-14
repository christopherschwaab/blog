NAME=post

all: $(NAME).html

p: $(NAME).html
	juice $(NAME).html $(NAME)-inline.html

post.cpp:
	sed -n '/~~~~~{.cpp}/,/~~~~~/p' $(NAME).md |grep -v '~~~~~' > post.cpp

$(NAME).html:
	pandoc -s --highlight-style pygments --bibliography references.bib --csl ../acm-sig-proceedings.csl $(NAME).md > $(NAME).html

clean:
	rm -f $(NAME).html $(NAME).wp.md $(NAME).wp.html

.PHONY: $(NAME).html all p clean
