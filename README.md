# Run

For me, anyway...

The ruby version lives in `.tool-versions`, so asdf picks it up automatically in
this directory:

```bash
asdf install          # installs the ruby in .tool-versions
bundle install
bundle exec jekyll serve
```

`bundle exec jekyll build` writes the site to `_site` instead of serving it.

## If the ruby build fails on Arch

`asdf install ruby` bootstraps the build with the system ruby, invoked as
`/usr/bin/ruby --disable=gems`. Arch ships `erb` as a separate package rather
than in the stdlib, and with gems disabled a gem-installed copy is invisible
too, so the build dies with `cannot load such file -- erb`. Either:

```bash
sudo pacman -S ruby-erb
```

or, without root, install the gem and hand the build a wrapper that puts it on
the load path:

```bash
gem install --user-install erb

cat > /tmp/baseruby <<EOF
#!/bin/sh
exec /usr/bin/ruby $(/usr/bin/ruby -e 'gem "erb"; require "erb"; puts $LOAD_PATH.grep(/erb-/).map { |p| "-I#{p}" }.join(" ")') "\$@"
EOF
chmod +x /tmp/baseruby

RUBY_CONFIGURE_OPTS="--with-baseruby=/tmp/baseruby" asdf install ruby
```

## New post

```bash
./scripts/post.bash "Title of the post"
```
