%% -------------------------------------------------------------------
%% This is a generated file.
%% -------------------------------------------------------------------

-module(diameter_3gpp_base).

-compile({parse_transform, diameter_exprecs}).

-compile(nowarn_unused_function).

-dialyzer(no_return).

-export_records([]).

-export([name/0,
         id/0,
         vendor_id/0,
         vendor_name/0,
         decode_avps/3,
         encode_avps/3,
         grouped_avp/4,
         msg_name/2,
         msg_header/1,
         rec2msg/1,
         msg2rec/1,
         name2rec/1,
         avp_name/2,
         avp_arity/1,
         avp_arity/2,
         avp_header/1,
         avp/4,
         enumerated_avp/3,
         empty_value/2,
         dict/0]).

-include_lib("diameter/include/diameter.hrl").

-include_lib("diameter/include/diameter_gen.hrl").

name() -> diameter_3gpp_base.

id() -> 0.

vendor_id() -> 10415.

vendor_name() -> '3GPP'.

msg_name(_, _) -> ''.

msg_header(_) -> erlang:error(badarg).

rec2msg(_) -> erlang:error(badarg).

msg2rec(_) -> erlang:error(badarg).

name2rec(T) -> msg2rec(T).

avp_name(27, 10415) ->
    {'3GPP-Allocate-IP-Type', 'OctetString'};
avp_name(24, 10415) ->
    {'3GPP-CAMEL-Charging-Info', 'OctetString'};
avp_name(4, 10415) ->
    {'3GPP-CG-Address', 'OctetString'};
avp_name(14, 10415) ->
    {'3GPP-CG-IPv6-Address', 'OctetString'};
avp_name(13, 10415) ->
    {'3GPP-Charging-Characteristics', 'UTF8String'};
avp_name(2, 10415) ->
    {'3GPP-Charging-Id', 'Unsigned32'};
avp_name(7, 10415) ->
    {'3GPP-GGSN-Address', 'OctetString'};
avp_name(16, 10415) ->
    {'3GPP-GGSN-IPv6-Address', 'OctetString'};
avp_name(9, 10415) ->
    {'3GPP-GGSN-MCC-MNC', 'UTF8String'};
avp_name(5, 10415) ->
    {'3GPP-GPRS-Negotiated-QoS-Profile', 'UTF8String'};
avp_name(20, 10415) -> {'3GPP-IMEISV', 'OctetString'};
avp_name(1, 10415) -> {'3GPP-IMSI', 'UTF8String'};
avp_name(8, 10415) ->
    {'3GPP-IMSI-MCC-MNC', 'UTF8String'};
avp_name(17, 10415) ->
    {'3GPP-IPv6-DNS-Servers', 'OctetString'};
avp_name(23, 10415) ->
    {'3GPP-MS-TimeZone', 'OctetString'};
avp_name(10, 10415) -> {'3GPP-NSAPI', 'OctetString'};
avp_name(26, 10415) ->
    {'3GPP-Negotiated-DSCP', 'OctetString'};
avp_name(3, 10415) -> {'3GPP-PDP-Type', 'Enumerated'};
avp_name(25, 10415) ->
    {'3GPP-Packet-Filter', 'OctetString'};
avp_name(21, 10415) -> {'3GPP-RAT-Type', 'OctetString'};
avp_name(6, 10415) ->
    {'3GPP-SGSN-Address', 'OctetString'};
avp_name(15, 10415) ->
    {'3GPP-SGSN-IPv6-Address', 'OctetString'};
avp_name(18, 10415) ->
    {'3GPP-SGSN-MCC-MNC', 'UTF8String'};
avp_name(12, 10415) ->
    {'3GPP-Selection-Mode', 'UTF8String'};
avp_name(11, 10415) ->
    {'3GPP-Session-Stop-Indicator', 'OctetString'};
avp_name(22, 10415) ->
    {'3GPP-User-Location-Info', 'OctetString'};
avp_name(30, 10415) ->
    {'3GPP-User-Location-Info-Time', 'Unsigned32'};
avp_name(29, 10415) ->
    {'TWAN-Identifier', 'OctetString'};
avp_name(_, _) -> 'AVP'.

avp_arity(_) -> erlang:error(badarg).

avp_arity(_, _) -> 0.

avp_header('3GPP-Allocate-IP-Type') -> {27, 128, 10415};
avp_header('3GPP-CAMEL-Charging-Info') ->
    {24, 128, 10415};
avp_header('3GPP-CG-Address') -> {4, 128, 10415};
avp_header('3GPP-CG-IPv6-Address') -> {14, 128, 10415};
avp_header('3GPP-Charging-Characteristics') ->
    {13, 128, 10415};
avp_header('3GPP-Charging-Id') -> {2, 128, 10415};
avp_header('3GPP-GGSN-Address') -> {7, 128, 10415};
avp_header('3GPP-GGSN-IPv6-Address') ->
    {16, 128, 10415};
avp_header('3GPP-GGSN-MCC-MNC') -> {9, 128, 10415};
avp_header('3GPP-GPRS-Negotiated-QoS-Profile') ->
    {5, 128, 10415};
avp_header('3GPP-IMEISV') -> {20, 128, 10415};
avp_header('3GPP-IMSI') -> {1, 128, 10415};
avp_header('3GPP-IMSI-MCC-MNC') -> {8, 128, 10415};
avp_header('3GPP-IPv6-DNS-Servers') -> {17, 128, 10415};
avp_header('3GPP-MS-TimeZone') -> {23, 128, 10415};
avp_header('3GPP-NSAPI') -> {10, 128, 10415};
avp_header('3GPP-Negotiated-DSCP') -> {26, 128, 10415};
avp_header('3GPP-PDP-Type') -> {3, 128, 10415};
avp_header('3GPP-Packet-Filter') -> {25, 128, 10415};
avp_header('3GPP-RAT-Type') -> {21, 128, 10415};
avp_header('3GPP-SGSN-Address') -> {6, 128, 10415};
avp_header('3GPP-SGSN-IPv6-Address') ->
    {15, 128, 10415};
avp_header('3GPP-SGSN-MCC-MNC') -> {18, 128, 10415};
avp_header('3GPP-Selection-Mode') -> {12, 128, 10415};
avp_header('3GPP-Session-Stop-Indicator') ->
    {11, 128, 10415};
avp_header('3GPP-User-Location-Info') ->
    {22, 128, 10415};
avp_header('3GPP-User-Location-Info-Time') ->
    {30, 128, 10415};
avp_header('TWAN-Identifier') -> {29, 128, 10415};
avp_header(_) -> erlang:error(badarg).

avp(T, Data, '3GPP-Allocate-IP-Type', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-CAMEL-Charging-Info', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-CG-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-CG-IPv6-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-Charging-Characteristics', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, '3GPP-Charging-Id', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, '3GPP-GGSN-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-GGSN-IPv6-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-GGSN-MCC-MNC', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, '3GPP-GPRS-Negotiated-QoS-Profile',
    Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, '3GPP-IMEISV', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-IMSI', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, '3GPP-IMSI-MCC-MNC', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, '3GPP-IPv6-DNS-Servers', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-MS-TimeZone', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-NSAPI', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-Negotiated-DSCP', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-PDP-Type', _) ->
    enumerated_avp(T, '3GPP-PDP-Type', Data);
avp(T, Data, '3GPP-Packet-Filter', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-RAT-Type', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-SGSN-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-SGSN-IPv6-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-SGSN-MCC-MNC', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, '3GPP-Selection-Mode', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, '3GPP-Session-Stop-Indicator', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-User-Location-Info', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, '3GPP-User-Location-Info-Time', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'TWAN-Identifier', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(_, _, _, _) -> erlang:error(badarg).

enumerated_avp(decode, '3GPP-PDP-Type',
               <<0, 0, 0, 0>>) ->
    0;
enumerated_avp(encode, '3GPP-PDP-Type', 0) ->
    <<0, 0, 0, 0>>;
enumerated_avp(decode, '3GPP-PDP-Type',
               <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, '3GPP-PDP-Type', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, '3GPP-PDP-Type',
               <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, '3GPP-PDP-Type', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, '3GPP-PDP-Type',
               <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, '3GPP-PDP-Type', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, '3GPP-PDP-Type',
               <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, '3GPP-PDP-Type', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(_, _, _) -> erlang:error(badarg).

empty_value('3GPP-PDP-Type', _) -> <<0, 0, 0, 0>>;
empty_value(Name, Opts) -> empty(Name, Opts).

dict() ->
    [1,
     {avp_types,
      [{"3GPP-Allocate-IP-Type", 27, "OctetString", "V"},
       {"3GPP-CAMEL-Charging-Info", 24, "OctetString", "V"},
       {"3GPP-CG-Address", 4, "OctetString", "V"},
       {"3GPP-CG-IPv6-Address", 14, "OctetString", "V"},
       {"3GPP-Charging-Characteristics",
        13,
        "UTF8String",
        "V"},
       {"3GPP-Charging-Id", 2, "Unsigned32", "V"},
       {"3GPP-GGSN-Address", 7, "OctetString", "V"},
       {"3GPP-GGSN-IPv6-Address", 16, "OctetString", "V"},
       {"3GPP-GGSN-MCC-MNC", 9, "UTF8String", "V"},
       {"3GPP-GPRS-Negotiated-QoS-Profile",
        5,
        "UTF8String",
        "V"},
       {"3GPP-IMEISV", 20, "OctetString", "V"},
       {"3GPP-IMSI", 1, "UTF8String", "V"},
       {"3GPP-IMSI-MCC-MNC", 8, "UTF8String", "V"},
       {"3GPP-IPv6-DNS-Servers", 17, "OctetString", "V"},
       {"3GPP-MS-TimeZone", 23, "OctetString", "V"},
       {"3GPP-NSAPI", 10, "OctetString", "V"},
       {"3GPP-Negotiated-DSCP", 26, "OctetString", "V"},
       {"3GPP-PDP-Type", 3, "Enumerated", "V"},
       {"3GPP-Packet-Filter", 25, "OctetString", "V"},
       {"3GPP-RAT-Type", 21, "OctetString", "V"},
       {"3GPP-SGSN-Address", 6, "OctetString", "V"},
       {"3GPP-SGSN-IPv6-Address", 15, "OctetString", "V"},
       {"3GPP-SGSN-MCC-MNC", 18, "UTF8String", "V"},
       {"3GPP-Selection-Mode", 12, "UTF8String", "V"},
       {"3GPP-Session-Stop-Indicator", 11, "OctetString", "V"},
       {"3GPP-User-Location-Info", 22, "OctetString", "V"},
       {"3GPP-User-Location-Info-Time", 30, "Unsigned32", "V"},
       {"TWAN-Identifier", 29, "OctetString", "V"}]},
     {avp_vendor_id, []},
     {codecs, []},
     {command_codes, []},
     {custom_types, []},
     {define, []},
     {enum,
      [{"3GPP-PDP-Type",
        [{"IPv4", 0},
         {"PPP", 1},
         {"IPv6", 2},
         {"IPv4v6", 3},
         {"Non-IP", 4}]}]},
     {grouped, []},
     {id, 0},
     {import_avps, []},
     {import_enums, []},
     {import_groups, []},
     {inherits, []},
     {messages, []},
     {name, "diameter_3gpp_base"},
     {prefix, "diameter_3gpp"},
     {vendor, {10415, "3GPP"}}].


