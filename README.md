# Foxfeet

Foxfeet is a feed (RSS, Atom, and JSON Feed) discovery program.

## Usage

### Examples

#### Discover

```
foxfeet discover https://github.com/blog
```

```
- url: https://github.blog/feed/
  type: RSS
- url: https://github.blog/comments/feed/
  type: RSS
- url: https://github.blog/wp-json/wp/v2/pages/78933
  type: JSON
```

##### `--check`

```
foxfeet discover --check https://github.com/blog
```

```
- url: https://github.blog/feed/
  type: RSS
```

#### Preview

##### `--limit`

```
foxfeet preview --limit 1 https://github.com/jpvillaisaza.atom
```

```
- title: jpvillaisaza opened a pull request in foxfeet
  url: https://github.com/jpvillaisaza/foxfeet/pull/19
```

##### `--json`

```
foxfeet preview --json --limit 1 https://github.com/jpvillaisaza.atom | fx .
```

```
[
  {
    "date": null,
    "title": "jpvillaisaza opened a pull request in foxfeet",
    "url": "https://github.com/jpvillaisaza/foxfeet/pull/19"
  }
]
```
