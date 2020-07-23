# (WIP) Multipost

*This product is work in progress!*

Tool to upload specified markdown files to Qiita.

## Example Usage

### Post a New Article

Given a markdown file named `article.md` (with metadata for [Hakyll](https://jaspervdj.be/hakyll/)):

```markdown
---
title: My ChangeLog for age 31.
author: YAMAMOTO Yuji
date: April 16, 2020
qiita-tags: Tags Separated By Spaces
canonical-url:
...
---

Happy birthday to me!
```

Run the multipost command:

```bash
$ multipost \
    --url-placeholder '^canonical-url:(.*)$' \
    `# ^ Get the canonical URL. If the first captured group is blank or` \
    `#   just "qiita.com", the article is treated as not posted yet.` \
    \
    --title '^title:(.*)$' \
    `# ^ The first captured group is the title of the post.` \
    \
    --tags '^qiita-tags:(.*)$' \
    `# ^ The first captured group is the tags of the post.` \
    `#   NOTE: "tags" in Hakyll are separated by comma, so I specified it` \
    `#         in the separate field "qiita-tags".` \
    \
    --metadata '^---.+---' \
    `# ^ Strip the matched area before posting the article.` \
    `#   NOTE: This regex is matched by block mode: '.' matches` \
    `#         any characters including newline characters.` \
    \
    --qiita-access-token=$QIITA_ACCESS_TOKEN \
    \
    article.md \
    `# ^ The target article. You can specify multiple articles.`
```

Then, the article "My ChangeLog for age 31." is published on Qiita.

NOTE: *But you should NOT post such articles! Qiita is only for programming-related information!*.

In addition, the posted article's URL on Qiita is written at the place spacified by `--url-placeholder` option.

```markdown
---
title: My ChangeLog for age 31.
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: https://qiita.com/your_qiita_id/items/article_id
...
---

Happy birthday to me!
```

### Modify an Already Posted Article

With the instructions above, we've published `article.md` on Qiita, and recorded the article's URL on `article.md`.  
How can we update the article? The answer is simple: edit the contents (and possibly tags), then run `multipost` against `article.md` again!  
If the URL (extracted by the regex of the `--url-placeholder` option) is already written, `multipost` just updates the article on Qiita.