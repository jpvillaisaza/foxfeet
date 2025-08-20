# Foxfeet

Foxfeet is a feed (RSS, Atom, and JSON Feed) discovery program.

## Usage

### Examples

```
foxfeet https://github.com/blog
```

```
- https://github.blog/feed/ (application/rss+xml)
- https://github.blog/comments/feed/ (application/rss+xml)
- https://github.blog/wp-json/wp/v2/pages/78933 (application/json)
```

#### `--check`

```
foxfeet --check https://github.com/blog
```

```
- https://github.blog/feed/ (application/rss+xml)
- https://github.blog/comments/feed/ (application/rss+xml)
```
