markdown_test <- "# This is the title

Then we have a _bunch *of* text_. Some of it are in *italics*, some if it in
**bold**, and the text ![](test) might ~even be _on_ multiple~ lines. <span>WHAT</span>

![](testgrob)

> this is called a paragraph
>
> 1. a bullet point
> 2. a second tight point
>
> a second paragraph inside a quote

In the above we used a block quote to make a point. Other noteworthy features are
the ability to format text `as code`. Often code is long so you can put it in
a whole block like this:

```
ggplot(mtcars) +
  geom_point(aes(mpg, disp))
```

The above illustrates a couple of things:

* * *

1. markdown is flexible
2. you can have lists
    * The lists can have sub-lists

        1. And so on

           new paragraph

      > a block quote

3. back to the original list

## To sum it up

***This is great***"
