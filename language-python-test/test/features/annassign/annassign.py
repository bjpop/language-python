primes: List[int] = []

captain: str  # Note: no initial value!

class Starship:
    stats: Dict[str, int] = {}


header: str
kind: int
body: Optional[List[str]]
header, kind, body = message

def f():
    a: int
    print(a)  # raises UnboundLocalError
    # Commenting out the a: int makes it a NameError.


a: int
a: str  # Static type checker may or may not warn about this.


class BasicStarship:
    captain: str = 'Picard'               # instance variable with default
    damage: int                           # instance variable without default
    stats: ClassVar[Dict[str, int]] = {}  # class variable


class Starship:
    captain: str = 'Picard'
    damage: int
    stats: ClassVar[Dict[str, int]] = {}

    def __init__(self, damage: int, captain: str = None):
        self.damage = damage
        if captain:
            self.captain = captain  # Else keep the default

def hit(self):
    Starship.stats['hits'] = Starship.stats.get('hits', 0) + 1


class Cls:
    pass

c = Cls()
c.x: int = 0  # Annotates c.x with int.
c.y: int      # Annotates c.y with int.

d = {}
d['a']: int = 0  # Annotates d['a'] with int.
d['b']: int      # Annotates d['b'] with int.
