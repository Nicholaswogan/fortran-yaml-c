import yaml

try:
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader

def test_roundtrip():

    fil = open("../tests/test2.yaml",'r')
    test2 = yaml.load(fil,Loader=Loader)
    fil.close()

    fil = open("test2_copy.yaml",'r')
    test2_copy = yaml.load(fil,Loader=Loader)
    fil.close()

    assert test2 == test2_copy
    print("test_roundtrip passed!")

if __name__ == "__main__":
    test_roundtrip()