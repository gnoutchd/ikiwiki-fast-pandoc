# ikiwiki-fast-pandoc

An "external" (XML-RPC) [IkiWiki][] plugin for rendering Markdown
fragments with [Pandoc][], replacing the default [mdwn][] plugin.

[ikiwiki-pandoc][] has the same purpose as this plugin, and is easier
to install, more mature, and more featureful.  However, fast-pandoc
can be significantly faster, especially if you use directives like
`\[[!table]]` and `\[[!report]]` that generate many Markdown fragments
that IkiWiki will htmlize individually.  Whereas [ikiwiki-pandoc][]
will fork+exec the pandoc binary for every fragment, fast-pandoc
embeds pandoc in a co-process that runs alongside IkiWiki.  At
[SFLC][], switching to fast-pandoc sped up rebuilds of an internal
wiki by 25-30%.

[IkiWiki]: https://ikiwiki.info/
[Pandoc]: https://pandoc.org/
[mdwn]: https://ikiwiki.info/plugins/mdwn/
[ikiwiki-pandoc]: https://github.com/sciunto-org/ikiwiki-pandoc
[SFLC]: https://softwarefreedom.org

## Build and install

Install git and [Haskell Stack](http://haskellstack.org/), then do:

```
git clone git://code.softwarefreedom.org/git/fast-pandoc
cd fast-pandoc
stack build
```

To make fast-pandoc available to your wiki, locate its "extra library
and plugin directory" (the `libdir` option in the *.setup file),
create a `plugins` subdirectory (if it doesn't already exist), and
install the `fast-pandoc` binary into it.  (See also
<https://ikiwiki.info/plugins/install/>.)  Many wikis put their libdir
at `~/.ikiwiki`.

```
mkdir -p ~/.ikiwiki/plugins/
stack install --local-bin-path ~/.ikiwiki/plugins
```

Finally, add `fast-pandoc` to the `add_plugins` list:

```
ikiwiki --changesetup ~/wiki.setup --plugin fast-pandoc
```
