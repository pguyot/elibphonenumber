-module(phonenumber_nif).
-export([
    'phonenumber_util.get_supported_regions'/0,
    'phonenumber_util.is_alpha_number'/1,
    'phonenumber_util.convert_alpha_characters_in_number'/1,
    'phonenumber_util.normalize_digits_only'/1,
    'phonenumber_util.normalize_diallable_chars_only'/1,
    'phonenumber_util.get_national_significant_number'/1,
    'phonenumber_util.get_length_of_geograpical_area_code'/1,
    'phonenumber_util.get_length_of_national_destination_code'/1,
    'phonenumber_util.get_country_mobile_token'/1,
    'phonenumber_util.format'/2,
    'phonenumber_util.format_by_pattern'/3,
    'phonenumber_util.format_national_number_with_carrier_code'/2,
    'phonenumber_util.format_national_number_with_preferred_carrier_code'/2,
    'phonenumber_util.format_number_for_mobile_dialing'/3,
    'phonenumber_util.format_out_of_country_calling_number'/2,
    'phonenumber_util.format_in_original_format'/2,
    'phonenumber_util.format_out_of_country_keeping_alpha_chars'/2,
    'phonenumber_util.truncate_too_long_number'/1,
    'phonenumber_util.get_number_type'/1,
    'phonenumber_util.is_valid_number'/1,
    'phonenumber_util.is_valid_number_for_region'/2,
    'phonenumber_util.get_region_code_for_number'/1,
    'phonenumber_util.get_country_code_for_region'/1,
    'phonenumber_util.get_region_code_for_country_code'/1,
    'phonenumber_util.get_region_codes_for_country_calling_code'/1,
    'phonenumber_util.is_nanpa_country'/1,
    'phonenumber_util.get_ndd_prefix_for_region'/2,
    'phonenumber_util.is_possible_number_with_reason'/1,
    'phonenumber_util.is_possible_number'/1,
    'phonenumber_util.is_possible_number_for_string'/2,
    'phonenumber_util.get_example_number'/1,
    'phonenumber_util.get_example_number_for_type'/2,
    'phonenumber_util.get_example_number_for_non_geo_entity'/1,
    'phonenumber_util.parse'/2,
    'phonenumber_util.parse_and_keep_raw_input'/2,
    'phonenumber_util.is_number_match'/2,
    'phonenumber_util.is_number_match_with_two_strings'/2,
    'phonenumber_util.is_number_match_with_one_string'/2,
    
    'phonenumber_offline_geocoder.get_description_for_valid_number'/2,
    'phonenumber_offline_geocoder.get_description_for_valid_number'/3,
    'phonenumber_offline_geocoder.get_description_for_number'/2,
    'phonenumber_offline_geocoder.get_description_for_number'/3
    ]).

-include("include/libphonenumber.hrl").
-on_load(init/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions:

init() ->
    case erlang:system_info(smp_support) of
        true -> ok;
        false -> error_logger:warning_msg("No smp_support detected: the elibphonenumber nif may block the main scheduler, proceed with caution.~n")
    end,
    SoName = filename:join(priv_dir(), ?MODULE),
    case erlang:load_nif(filename:absname(SoName), 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, {upgrade, _}} -> ok
    end.

priv_dir() ->
    case code:priv_dir(libphonenumber) of
        PrivDir when is_list(PrivDir) ->
            PrivDir;
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(Ebin), "priv")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% phonenumber_util bindings

-spec 'phonenumber_util.get_supported_regions'() -> list(binary()) | no_return().

'phonenumber_util.get_supported_regions'() ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_alpha_number'(Number::binary()) -> boolean() | no_return().


'phonenumber_util.is_alpha_number'(_Number) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.convert_alpha_characters_in_number'(Number::binary()) -> binary() | no_return().


'phonenumber_util.convert_alpha_characters_in_number'(_Number) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.normalize_digits_only'(Number::binary()) -> binary() | no_return().


'phonenumber_util.normalize_digits_only'(_Number) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.normalize_diallable_chars_only'(Number::binary()) -> binary() | no_return().


'phonenumber_util.normalize_diallable_chars_only'(_Number) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_national_significant_number'(
    PhoneNumber::phonenumber()
    ) -> NationalSignificantNum::binary() | no_return().


'phonenumber_util.get_national_significant_number'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_length_of_national_destination_code'(
    PhoneNumber::phonenumber()
    ) -> non_neg_integer() | no_return().


'phonenumber_util.get_length_of_national_destination_code'(_PhoneNumber) -> 
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_length_of_geograpical_area_code'(
    PhoneNumber::phonenumber()
    ) -> non_neg_integer() | no_return().

'phonenumber_util.get_length_of_geograpical_area_code'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_country_mobile_token'(CountryCallingCode::non_neg_integer()) -> binary() | no_return().


'phonenumber_util.get_country_mobile_token'(_CountryCallingCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format'(
    PhoneNumber::phonenumber(), 
    PhoneNumberFormat::phonenumber_format()
    ) -> FormattedNumber::binary() | no_return().


'phonenumber_util.format'(_PhoneNumber, _PhoneNumberFormat) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format_by_pattern'(
    PhoneNumber::phonenumber(),
    PhoneNumberFormat::phonenumber_format(),
    UserDefinedFormats::list(phonenumber_format())
    ) -> FormattedNumber::binary() | no_return().


'phonenumber_util.format_by_pattern'(_PhoneNumber, _PhoneNumberFormat, _UserDefinedFormats) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format_national_number_with_carrier_code'(
    PhoneNumber::phonenumber(),
    CarrierCode::binary()
    ) -> FormattedNumber::binary() | no_return().


'phonenumber_util.format_national_number_with_carrier_code'(_PhoneNumber, _CarrierCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format_national_number_with_preferred_carrier_code'(
    PhoneNumber::phonenumber(),
    FallbackCarrierCode::binary()
    ) -> FormattedNumber::binary() | no_return().

'phonenumber_util.format_national_number_with_preferred_carrier_code'(_PhoneNumber, _FallbackCarrierCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format_number_for_mobile_dialing'(
    PhoneNumber::phonenumber(),
    RegionCallingFrom::binary(),
    WithFormatting::boolean()
    ) -> FormattedNumber::binary() | no_return().


'phonenumber_util.format_number_for_mobile_dialing'(_PhoneNumber, _RegionCallingFrom, _WithFormatting) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format_out_of_country_calling_number'(
    PhoneNumber::phonenumber(),
    CallingFrom::binary()
    ) -> FormattedNumber::binary() | no_return().

'phonenumber_util.format_out_of_country_calling_number'(_PhoneNumber, _CallingFrom) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format_in_original_format'(
    PhoneNumber::phonenumber(),
    RegionCallingFrom::binary()
    ) -> FormattedNumber::binary() | no_return().


'phonenumber_util.format_in_original_format'(_PhoneNumber, _RegionCallingFrom) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.format_out_of_country_keeping_alpha_chars'(
    PhoneNumber::phonenumber(),
    CallingFrom::binary()
    ) -> FormattedNumber::binary() | no_return().

'phonenumber_util.format_out_of_country_keeping_alpha_chars'(_PhoneNumber, _CallingFrom) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.truncate_too_long_number'(
    PhoneNumber::phonenumber()
    ) -> ValidPhoneNumber::phonenumber() | {error, no_valid_number} | no_return().


'phonenumber_util.truncate_too_long_number'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_number_type'(PhoneNumber::phonenumber()) -> phonenumber_type() | no_return().


'phonenumber_util.get_number_type'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_valid_number'(PhoneNumber::phonenumber()) -> boolean() | no_return().


'phonenumber_util.is_valid_number'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_valid_number_for_region'(
    PhoneNumber::phonenumber(),
    Region::binary()
    ) -> boolean() | no_return().


'phonenumber_util.is_valid_number_for_region'(_PhoneNumber, _Region) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_region_code_for_number'(
    PhoneNumber::phonenumber()
    ) -> RegionCode::binary() | no_return().


'phonenumber_util.get_region_code_for_number'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_country_code_for_region'(
    RegionCode::binary()
    ) -> CountryCode::non_neg_integer() | no_return().


'phonenumber_util.get_country_code_for_region'(_RegionCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_region_code_for_country_code'(
    CountryCode::non_neg_integer()
    ) -> RegionCode::binary() | no_return().


'phonenumber_util.get_region_code_for_country_code'(_CountryCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_region_codes_for_country_calling_code'(
    CountryCallingCode::non_neg_integer()
    ) -> list(binary()) | no_return().


'phonenumber_util.get_region_codes_for_country_calling_code'(_CountryCallingCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_nanpa_country'(RegionCode::binary()) -> boolean() | no_return().


'phonenumber_util.is_nanpa_country'(_RegionCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_ndd_prefix_for_region'(
    RegionCode::binary(), 
    StripNonDigits::boolean()
    ) -> NationalPrefix::binary() | no_return().


'phonenumber_util.get_ndd_prefix_for_region'(_RegionCode, _StripNonDigits) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_possible_number_with_reason'(
    PhoneNumber::phonenumber()
    ) -> ValidationResult::validation_result() | no_return().


'phonenumber_util.is_possible_number_with_reason'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_possible_number'(
    PhoneNumber::phonenumber()
    ) -> boolean() | no_return().


'phonenumber_util.is_possible_number'(_PhoneNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_possible_number_for_string'(
    Number::binary(),
    RegionDialingFrom::binary()
    ) -> boolean() | no_return().


'phonenumber_util.is_possible_number_for_string'(_Number, _RegionDialingFrom) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_example_number'(
    RegionCode::binary()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_region} | no_return().


'phonenumber_util.get_example_number'(_RegionCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_example_number_for_type'(
    RegionCode::binary(),
    PhoneNumberType::phonenumber_type()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_region} | no_return().


'phonenumber_util.get_example_number_for_type'(_RegionCode, _PhoneNumberType) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.get_example_number_for_non_geo_entity'(
    CountryCallingCode::binary()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_code} | no_return().


'phonenumber_util.get_example_number_for_non_geo_entity'(_CountryCallingCode) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.parse'(
    NumberToParse::binary(),
    DefaultRegion::binary()
    ) -> PhoneNumber::phonenumber() | {error, term()} | no_return().

'phonenumber_util.parse'(_NumberToParse, _DefaultRegion) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.parse_and_keep_raw_input'(
    NumberToParse::binary(),
    DefaultRegion::binary()
    ) -> PhoneNumber::phonenumber() | {error, term()} | no_return().


'phonenumber_util.parse_and_keep_raw_input'(_NumberToParse, _DefaultRegion) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_number_match'(
    FirstNumber::phonenumber(),
    SecondNumber::phonenumber()
    ) -> match_type() | no_return().

'phonenumber_util.is_number_match'(_FirstNumber, _SecondNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_number_match_with_two_strings'(
    FirstNumber::binary(),
    SecondNumber::binary()
    ) -> match_type() | no_return().


'phonenumber_util.is_number_match_with_two_strings'(_FirstNumber, _SecondNumber) ->
    erlang:nif_error(nif_library_not_loaded).

-spec 'phonenumber_util.is_number_match_with_one_string'(
    FirstNumber::phonenumber(),
    SecondNumber::binary()
    ) -> match_type() | no_return().


'phonenumber_util.is_number_match_with_one_string'(_FirstNumber, _SecondNumber) ->
    erlang:nif_error(nif_library_not_loaded).


-spec 'phonenumber_offline_geocoder.get_description_for_valid_number'(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()}
    ) -> unicode:unicode_binary() | no_return().


'phonenumber_offline_geocoder.get_description_for_valid_number'(_Number, _Locale) ->
    erlang:nif_error(nif_library_not_loaded).


-spec 'phonenumber_offline_geocoder.get_description_for_valid_number'(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()},
    UserRegion::iodata()
    ) -> unicode:unicode_binary() | no_return().


'phonenumber_offline_geocoder.get_description_for_valid_number'(_Number, _Locale, _UserRegion) ->
    erlang:nif_error(nif_library_not_loaded).


-spec 'phonenumber_offline_geocoder.get_description_for_number'(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()}
    ) -> unicode:unicode_binary() | no_return().


'phonenumber_offline_geocoder.get_description_for_number'(_Number, _Locale) ->
    erlang:nif_error(nif_library_not_loaded).


-spec 'phonenumber_offline_geocoder.get_description_for_number'(
    Number::phonenumber(),
    Locale::iodata() | atom() | {atom(), atom()},
    UserRegion::iodata()
    ) -> unicode:unicode_binary() | no_return().


'phonenumber_offline_geocoder.get_description_for_number'(_Number, _Locale, _UserRegion) ->
    erlang:nif_error(nif_library_not_loaded).
