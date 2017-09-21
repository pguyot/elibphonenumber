-module(phonenumber_offline_geocoder).
-export([
    get_description_for_valid_number/2,
    get_description_for_valid_number/3,
    get_description_for_number/2,
    get_description_for_number/3
    ]).

-include("include/libphonenumber.hrl").

-spec get_description_for_valid_number(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()}
    ) -> unicode:unicode_binary() | no_return().


get_description_for_valid_number(Number, Locale) ->
    phonenumber_nif:'phonenumber_offline_geocoder.get_description_for_valid_number'(Number, Locale).


-spec get_description_for_valid_number(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()},
    UserRegion::iodata()
    ) -> unicode:unicode_binary() | no_return().


get_description_for_valid_number(Number, Locale, UserRegion) ->
    phonenumber_nif:'phonenumber_offline_geocoder.get_description_for_valid_number'(Number, Locale, UserRegion).


-spec get_description_for_number(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()}
    ) -> unicode:unicode_binary() | no_return().


get_description_for_number(Number, Locale) ->
    phonenumber_nif:'phonenumber_offline_geocoder.get_description_for_number'(Number, Locale).


-spec get_description_for_number(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()},
    UserRegion::iodata()
    ) -> unicode:unicode_binary() | no_return().


get_description_for_number(Number, Locale, UserRegion) ->
    phonenumber_nif:'phonenumber_offline_geocoder.get_description_for_number'(Number, Locale, UserRegion).


