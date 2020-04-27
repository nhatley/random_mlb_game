# Pick random games from MLB TV
  
The goal of  this site is to allow for building a random url for an (mlb.tv) game. 

When watching games myself I find that it'd be better if I didn't look for a specific game to watch because I often remember the outcome or something about the game I end up picking.

So, I wanted to build something that can take a few inputs (e.g. year, team, vs division etc?) and then randomly select a game from that filtered list and return the mlb tv url.

## Development Notes: 

The obv biggest thing here will be to figure out the hashes and ways that mlbtv builds its links

I discovered that all you need to get to a game is just the following:



For example, for Braves/Mets 06/19/19 game:
https://www.mlb.com/tv/g567207

So, now I just need to collect these game_ids (e.g. g567207) and attach them to games.

9a802de