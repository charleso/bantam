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

### V0 - Login

```
/current
  - {fight}

/user/{user}/password

/session/{session}
  - {user}
```

### V1 - Lemmas

```
/fight/{fight}/lemma/{lemma}

/fight/{fight}/fighter/{user}/lemma/{lemma}
```

### V2 - Inbox

```
/user/{user}/fight/{fight}/inbox/{lemma}

/fight/{fight}/review/{lemma}/{user}
```

### V4 - Current

```
/fight/{fight}/current-lemma
  - {lemma}
```

### V5 - Rounds

```
/user/{user}/fight/{fight}/round/{round}

/fight/{fight}/round/{round}/data
  - title
  - {lemma}
/fight/{fight}/round/{round}/fighter/{user}
/fight/{fight}/round/{round}/result
  - {user}
```
