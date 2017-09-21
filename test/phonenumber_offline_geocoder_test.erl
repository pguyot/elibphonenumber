-module(phonenumber_offline_geocoder_test).

-include("include/libphonenumber.hrl").
-include_lib("eunit/include/eunit.hrl").

make_number(CountryCode, Number) ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(CountryCode, P1),
    P12 = phonenumber:set_national_number(Number, P11),
    P12.

-define(KO_NUMBER1, make_number(82, 22123456)).
-define(KO_NUMBER2, make_number(82, 322123456)).
-define(KO_NUMBER3, make_number(82, 6421234567)).
-define(KO_INVALID_NUMBER, make_number(82, 1234)).
-define(KO_MOBILE, make_number(82, 101234567)).
-define(US_NUMBER1, make_number(1, 6502530000)).
-define(US_NUMBER2, make_number(1, 6509600000)).
-define(US_NUMBER3, make_number(1, 2128120000)).
-define(US_NUMBER4, make_number(1, 6174240000)).
-define(US_INVALID_NUMBER, make_number(1, 123456789)).
-define(BS_NUMBER1, make_number(1, 2423651234)).
-define(AU_NUMBER, make_number(61, 236618300)).
-define(NUMBER_WITH_INVALID_COUNTRY_CODE, make_number(999, 2423651234)).
-define(INTERNATIONAL_TOLL_FREE, make_number(800, 12345678)).

% Unlike cpp version, all tests here are performed on production database.

get_description_for_number_test_() ->
    [
        ?_assertEqual(<<"\xe7""\xbe""\x8e""\xe5""\x9b""\xbd">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER1, {zh, 'CN'})),
        ?_assertEqual(<<"Abaco Island">>, phonenumber_offline_geocoder:get_description_for_number(?BS_NUMBER1, {en, 'US'})),
        ?_assertEqual(<<"Australia">>, phonenumber_offline_geocoder:get_description_for_number(?AU_NUMBER, {en, 'US'})),
        ?_assertEqual(<<"">>, phonenumber_offline_geocoder:get_description_for_number(?NUMBER_WITH_INVALID_COUNTRY_CODE, {en, 'US'})),
        ?_assertEqual(<<"">>, phonenumber_offline_geocoder:get_description_for_number(?INTERNATIONAL_TOLL_FREE, {en, 'US'}))
    ].

get_description_for_number_en_US_test_() ->
    [
        ?_assertEqual(<<"Mountain View, CA">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER1, {en, 'US'})),
        ?_assertEqual(<<"Mountain View, CA">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER2, {en, 'US'})),
        ?_assertEqual(<<"New York, NY">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER3, {en, 'US'}))
    ].

get_description_for_korean_number_test_() ->
    [
        ?_assertEqual(<<"Seoul">>, phonenumber_offline_geocoder:get_description_for_number(?KO_NUMBER1, {en, 'US'})),
        ?_assertEqual(<<"Incheon">>, phonenumber_offline_geocoder:get_description_for_number(?KO_NUMBER2, {en, 'US'})),
        ?_assertEqual(<<"Jeju">>, phonenumber_offline_geocoder:get_description_for_number(?KO_NUMBER3, {en, 'US'})),
        ?_assertEqual(<<"서울"/utf8>>, phonenumber_offline_geocoder:get_description_for_number(?KO_NUMBER1, {ko, 'KR'})),
        ?_assertEqual(<<"인천"/utf8>>, phonenumber_offline_geocoder:get_description_for_number(?KO_NUMBER2, {ko, 'KR'}))
    ].

get_description_for_fallback_test_() ->
% No fallback, as the location name for the given phone number is available
% in the requested language.
    [
% German falls back to English.
        ?_assertEqual(<<"New York, NY">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER3, {de, 'DE'})),
% Italian falls back to English.
        ?_assertEqual(<<"Mountain View, CA">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER1, {it, 'IT'})),
% Korean doesn't fall back to English.
        ?_assertEqual(<<"제주"/utf8>>, phonenumber_offline_geocoder:get_description_for_number(?KO_NUMBER3, {ko, 'KR'}))
    ].

get_description_for_number_with_user_region_test_() ->
    [
% User in Italy, American number. We should just show United States, in
% Spanish, and not more detailed information.
        ?_assertEqual(<<"Estados Unidos">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER1, {es, 'ES'}, "IT")),
% Unknown region - should just show country name.
        ?_assertEqual(<<"Estados Unidos">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER1, {es, 'ES'}, "ZZ")),
% User in the States, language German, should show detailed data.
        ?_assertEqual(<<"Mountain View, CA">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER1, {de, 'DE'}, "US")),
% User in the States, language French, no data for French, so we fallback to
% English detailed data.
        ?_assertEqual(<<"Mountain View, CA">>, phonenumber_offline_geocoder:get_description_for_number(?US_NUMBER1, {fr, 'FR'}, "US")),
% Invalid number - return an empty string.
        ?_assertEqual(<<"">>, phonenumber_offline_geocoder:get_description_for_number(?US_INVALID_NUMBER, {en, 'US'}, "US"))
    ].

get_description_for_invalid_number_test_() ->
    [
        ?_assertEqual(<<"">>, phonenumber_offline_geocoder:get_description_for_number(?KO_INVALID_NUMBER, {en, 'US'})),
        ?_assertEqual(<<"">>, phonenumber_offline_geocoder:get_description_for_number(?US_INVALID_NUMBER, {en, 'US'}))
    ].

get_description_for_non_geographical_number_with_geocoding_prefix_test_() ->
% We have a geocoding prefix, but we shouldn't use it since this is not
% geographical.
    [
        ?_assertEqual(<<"South Korea">>, phonenumber_offline_geocoder:get_description_for_number(?KO_MOBILE, {en, 'US'}))
    ].
