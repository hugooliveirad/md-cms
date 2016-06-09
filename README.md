# md-cms
Haskell course project of a cms (FATEC-BS)

Technologies used:
- Haskell + Yesod + Postgres (backend)
- Elm (frontend)

## MER (Modelo Entidade Relacionamento)

*relationship diagram*

![mer-md-cms](https://raw.githubusercontent.com/hugobessaa/md-cms/master/mer-md-cms.png)


## Interact

Running application: https://haskell-yesod-hugobessaa.c9users.io/

- POST a Tag

    ```bash
    curl -XPOST https://haskell-yesod-hugobessaa.c9users.io/api/tags -d '{"name": "GitHub Post", "authorId": 5}'
    ```

- PUT a Collection

    ```bash
    curl -XPUT https://haskell-yesod-hugobessaa.c9users.io/api/collections/3 -d '{"name": "Techy", "authorId": 5}'
    ```

- POST a CollectionPost (creates the relationship)

    ```bash
    curl -XPOST https://haskell-yesod-hugobessaa.c9users.io/api/collections/3/posts/4 
    ```

See `src/Handlers.hs` for a glimpse of what you can do.
