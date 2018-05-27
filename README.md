Bantam
======

[![Build Status](https://travis-ci.org/charleso/bantam.svg?branch=master)](https://travis-ci.org/charleso/bantam)

> 1. small or miniature chickens or ducks
>
> 2. a weight class in boxing and mixed martial arts
>
> https://en.wikipedia.org/wiki/Bantam

![coq](https://avatars0.githubusercontent.com/u/621198?v=3&s=400)

Website for managing Coq fights


## Store

```
/current
  - {fight}

/user/{user}/password

/session/{session}
  - {user}

/fight/{fight}/lemma/{lemma}

/fight/{fight}/fighter/{user}/lemma/{lemma}

/fight/{fight}/fighter/{user}/inbox/{lemma}

/fight/{fight}/review/{lemma}/{user}

/fight/{fight}/matches
```

### Matches format

The matches file has the following (horrible) format.

```
player1|player2|game results|current lemma|upcoming lemmas
```

An example:

```
player1@foo.com|player2@bar.com|lemma1:D,lemma2:W1|lemma3|lemma4,lemma5
player2@foo.com|player3@bar.com||||lemma6,lemma7
```

- `D` is a draw
- `W1` is a win for the first player
- `W2` is a win for the second player
