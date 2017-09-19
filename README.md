elibphonenumber
===============

Erlang NIF for [libphonenumber](https://github.com/googlei18n/libphonenumber).

## Note

This repository is a fork of [johnhamelink/elibphonenumber][4] which itself is a fork of [silviucpp/elibphonenumber][3].

Compared to upstream version, this repository includes recent from [silviucpp/elibphonenumber][3], including `phonenumber_to_carrier` module, as well as a fix to compile elibphonenumber on FreeBSD.

## Dependencies

You need compile `libphonenumber` and install it before compiling the project. You can read up on how people have done this on the [wiki.][2]

## Use with mix

```elixir
defp deps do
  [{:elibphonenumber, git: "git://github.com/pguyot/elibphonenumber.git"}]
end
```

## Run the tests

```sh
rebar3 compile
rebar3 eunit
```

[1]:https://www.wowapp.com/w/silviu/Silviu-Caragea
[2]:https://github.com/johnhamelink/elibphonenumber/wiki/Compiling-Libphonenumber
[3]:https://github.com/silviucpp/elibphonenumber
[4]:https://github.com/johnhamelink/elibphonenumber