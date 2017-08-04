# csv-zipcode-fix
Fixes two-letter state codes in a spreadsheet based on zip code data in the
same spreadsheet. I wrote this as a quick and dirty fix for a client, but
thought it might be useful again someday.

Creating a full FromNamedRecord instance for a row ends up being a pretty
high-powered solution. The downside is that it requires a lot of boilerplate
code... Vim macros came in handy here.

It's handy that the [cassava](https://hackage.haskell.org/package/cassava)
CSV-parsing package uses the same syntax as Aeson and YAML for creating
parseable data types. `.:` is used as a lookup function, and since the 
parsers are functors, we can just use `<$>` and `<*>` to map a type 
constructor over the parsers.

Here, `Transaction <$> (p .: "ID") <*> p .: "Payment ID"`...etc... is the
Transaction constructor (`String -> String -> String ...etc... -> Transaction`)
getting fmap'd over a bunch of parsers.

`<$>` is infix `fmap`. `fmap` has type `Functor f => (a -> b) -> f a -> f b`.
in this situation, our `a -> b` is the `Transaction` constructor, so:

* our `a` is `(String -> String -> String ... etc... -> String)`
* our `b` is `Transaction`
* our `f` is `Parser` from the cassava package.

So we end up with `Parser Transaction`, which is kind of neat. Anyway, I'm just
writing this out so that future-me understands what the hell kind of type magic
is going on here, and maybe someone else on the internet will stumble across
this someday.
