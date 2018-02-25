# Corpus-DB

Corpus-DB is a textual corpus database for the digital humanities. This project aggregates public domain texts, enhances their metadata from sources like Wikipedia, and makes those texts available according to that metadata. This will make it easy to download subcorpora like: 

 - Bildungsromans
 - Dickens novels
 - Poetry published in the 1880s
 - Novels set in London

Corpus-DB has several components: 

 - Scripts for aggregating metadata, written in Python
 - The database, currently a few SQLite databases
 - A REST API for querying the database, written in Haskell (currently in progress)
 - Analytic experiments, mostly in Python
 
Read more about the database [at this introductory blog post](http://jonreeve.com/2017/06/project-gutenberg-the-database/). Scripts used to generate the database are in the [gitenberg-experiments repo](https://github.com/JonathanReeve/gitenberg-experiments). 

# Contributing

I could use some help with this, especially if you know Python or Haskell, have library or bibliography experience, or simply like books. Get in touch in the chat room, or [contact me via email](mailto:jon.reeve@gmail.com). [![Join the chat at https://gitter.im/corpus-db/Lobby](https://badges.gitter.im/corpus-db/Lobby.svg)](https://gitter.im/corpus-db/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Hacking

If you want to build the website and API, you'll need the Haskell tool `stack`. 

```
stack build
cd src
stack runhaskell Main.hs
```

At the moment, you'll probably want to check the port that Scotty is serving to, and make sure the path to the database (`db`) is correct. This might change soon, as I introduce development and production environments.
