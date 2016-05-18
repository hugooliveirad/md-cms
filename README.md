# md-cms
haskell course project of a cms

**wip**

## DB

Authors have an ´id´, ´name´, ´nick´, ´password´

Posts have an ´id´, ´title´, ´content´, ´publish_date´, ´author_id´

Tags have an ´id´, ´name´, ´author_id´

Collections have an ´id´, ´title´, ´author_id´

PostsTags have an ´id´, ´post_id´, ´tag_id´

PostsCollections have an ´id´, ´collection_id´, ´post_id´, ´author_id´

## Use cases

Unlogged users can create an Author
Unlogged users log as an Author

Authors can CRUD their's Posts
Authors can CRUD their's Tags
Authors can CRUD their's Collections

Tags are public, but can't be modified by anyone but it's author
Collections are public, but can't be modified by anyone but it's author
Posts are public, but can't be modified by anyone but it's author

Collections can include posts by other authors
