# Pick random games from MLB TV
  
The goal of  this site is to allow for building a random url for an (mlb.tv) game. 

When watching games myself I find that it'd be better if I didn't look for a specific game to watch because I often remember the outcome or something about the game I end up picking.

So, I wanted to build something that can take a few inputs (e.g. year, team, vs division etc?) and then randomly select a game from that filtered list and return the mlb tv url.

## Development Notes: 

The obv biggest thing here will be to figure out the hashes and ways that mlbtv builds its links

Braves/Cards NLDS game 3 link:

"https://www.mlb.com/tv/g599354/v42509576-81de-497d-bc3a-8e0e1f0920a4#game=599354,tfs=20191006_201000,game_state=preview"

