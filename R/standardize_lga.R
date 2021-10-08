#' Standarize LGA Names
#'
#' @param x Character vector of names, probably LGA names.
#' @param .dt (Internal use only, whether to return a \code{data.table} instead of a character vector.)
#'
#' @return The character vector \code{x} but with best-guess standard names, where the LGA
#' in \code{x} was not present in the LGA 2016 series.
#'
#' @export

standardize_lga <- function(x) {
  lga_names16 <- get_lga_names16(.dt = FALSE)
  if (all(x %chin% lga_names16, na.rm = TRUE)) {
    # TODO: no standardize
    return(x)
  }

  out <-
    Switch(x,

           "Kingston (C)" = "Kingston (C) (Vic.)",  # alternative would be (DC) (SA)
           "Latrobe (C)" =  "Latrobe (C) (Vic.)",   # alternative would be Latrobe (M) (Tas.)
           "Wodonga (RC)" = "Wodonga (C)",
           "Overseas",
           "Unknown",
           "Tumbarumba (A)" = "Snowy Valleys (A)",
           "Central Highlands (R)",
           "Marrickville (A)",
           "Armidale Dumaresq (A)",
           "Interstate",
           "Snowy River (A)" = "Snowy Valleys (A)",
           "Murray (A)" = "Murray River (A)",
           "Deniliquin (A)",
           "Wakool (A)",
           "Campbelltown (C)",
           "Young (A)",
           "Warringah (A)",
           "Queanbeyan (C)",
           "Auburn (C)",
           "Pittwater (A)",
           "Tumut Shire (A)",
           "Hurstville (C)",
           "Canterbury (C)",
           "Conargo (A)",
           "Holroyd (C)",
           "Gosford (C)",
           "Bankstown (C)",
           "Boorowa (A)",
           "Wyong (A)",
           "Central Coast (M)",
           "Latrobe (M)",
           "Leichhardt (A)",
           "Central Highlands (M)",
           "Harden (A)",
           "Wellington (A)",
           "Corowa Shire (A)",
           "Urana (A)",
           "Kogarah (C)",
           "Ashfield (A)",
           "Manly (A)",
           "Great Lakes (A)",
           "Greater Taree (C)",
           "Cooma-Monaro (A)" = "Snowy Monaro Regional (A)",
           "Dubbo (C)",
           "Jerilderie (A)",
           "Palerang (A)",
           "Guyra (A)",
           "Cootamundra (A)",
           "Bombala (A)" = "Snowy Monaro Regional (A)",
           "Gloucester (A)",
           DEFAULT = x)




}

#' @rdname standardize_lga
#' @export
encode_lga <- function(lga) {
  fmatch(standardize_lga(x), get_lga_names16(FALSE))
}


#' @rdname standardize_lga
#' @export
get_lga_names16 <- function(.dt = TRUE) {
  if (.dt) {
    LGA <- LGA_INTRNL_ID <- NULL
    out <- data.table(LGA = get_lga_names16(.dt = FALSE))
    out[, "LGA_INTRNL_ID" := seq_len(.N)]
    return(out[])
  }

  # ordering is preserved from ASGS::LGA_2016_Centroids
  # was a factor there -- possibly useful to preserve order
  c("Albury (C)", "Armidale Regional (A)", "Blayney (A)", "Singleton (A)",
    "Snowy Monaro Regional (A)", "Snowy Valleys (A)", "Strathfield (A)",
    "Sutherland Shire (A)", "Sydney (C)", "Tamworth Regional (A)",
    "Temora (A)", "Tenterfield (A)", "The Hills Shire (A)", "Blue Mountains (C)",
    "Tweed (A)", "Upper Hunter Shire (A)", "Upper Lachlan Shire (A)",
    "Uralla (A)", "Wagga Wagga (C)", "Walcha (A)", "Walgett (A)",
    "Warren (A)", "Warrumbungle Shire (A)", "Waverley (A)", "Bogan (A)",
    "Weddin (A)", "Wentworth (A)", "Western Plains Regional (A)",
    "Willoughby (C)", "Wingecarribee (A)", "Wollondilly (A)", "Wollongong (C)",
    "Woollahra (A)", "Yass Valley (A)", "Unincorporated NSW", "Botany Bay (C)",
    "Alpine (S)", "Ararat (RC)", "Ballarat (C)", "Banyule (C)", "Bass Coast (S)",
    "Baw Baw (S)", "Bayside (C)", "Benalla (RC)", "Bourke (A)", "Boroondara (C)",
    "Brimbank (C)", "Buloke (S)", "Campaspe (S)", "Cardinia (S)",
    "Casey (C)", "Central Goldfields (S)", "Colac-Otway (S)", "Corangamite (S)",
    "Darebin (C)", "Brewarrina (A)", "East Gippsland (S)", "Frankston (C)",
    "Gannawarra (S)", "Glen Eira (C)", "Glenelg (S)", "Golden Plains (S)",
    "Greater Bendigo (C)", "Greater Dandenong (C)", "Greater Geelong (C)",
    "Greater Shepparton (C)", "Broken Hill (C)", "Hepburn (S)", "Hindmarsh (S)",
    "Hobsons Bay (C)", "Horsham (RC)", "Hume (C)", "Indigo (S)",
    "Kingston (C) (Vic.)", "Knox (C)", "Latrobe (C) (Vic.)", "Loddon (S)",
    "Burwood (A)", "Macedon Ranges (S)", "Manningham (C)", "Mansfield (S)",
    "Maribyrnong (C)", "Maroondah (C)", "Melbourne (C)", "Melton (C)",
    "Mildura (RC)", "Mitchell (S)", "Moira (S)", "Byron (A)", "Monash (C)",
    "Moonee Valley (C)", "Moorabool (S)", "Moreland (C)", "Mornington Peninsula (S)",
    "Mount Alexander (S)", "Moyne (S)", "Murrindindi (S)", "Nillumbik (S)",
    "Northern Grampians (S)", "Cabonne (A)", "Port Phillip (C)",
    "Pyrenees (S)", "Queenscliffe (B)", "South Gippsland (S)", "Southern Grampians (S)",
    "Stonnington (C)", "Strathbogie (S)", "Surf Coast (S)", "Swan Hill (RC)",
    "Towong (S)", "Ballina (A)", "Camden (A)", "Wangaratta (RC)",
    "Warrnambool (C)", "Wellington (S)", "West Wimmera (S)", "Whitehorse (C)",
    "Whittlesea (C)", "Wodonga (C)", "Wyndham (C)", "Yarra (C)",
    "Yarra Ranges (S)", "Campbelltown (C) (NSW)", "Yarriambiack (S)",
    "Unincorporated Vic", "Aurukun (S)", "Balonne (S)", "Banana (S)",
    "Barcaldine (R)", "Barcoo (S)", "Blackall-Tambo (R)", "Canada Bay (A)",
    "Boulia (S)", "Brisbane (C)", "Bulloo (S)", "Bundaberg (R)",
    "Burdekin (S)", "Burke (S)", "Cairns (R)", "Carpentaria (S)",
    "Cassowary Coast (R)", "Central Highlands (R) (Qld)", "Canterbury-Bankstown (A)",
    "Charters Towers (R)", "Cherbourg (S)", "Cloncurry (S)", "Cook (S)",
    "Croydon (S)", "Diamantina (S)", "Doomadgee (S)", "Douglas (S)",
    "Etheridge (S)", "Flinders (S) (Qld)", "Carrathool (A)", "Fraser Coast (R)",
    "Gladstone (R)", "Gold Coast (C)", "Goondiwindi (R)", "Gympie (R)",
    "Hinchinbrook (S)", "Hope Vale (S)", "Ipswich (C)", "Isaac (R)",
    "Kowanyama (S)", "Central Coast (C) (NSW)", "Livingstone (S)",
    "Lockhart River (S)", "Lockyer Valley (R)", "Logan (C)", "Longreach (R)",
    "Mackay (R)", "McKinlay (S)", "Mapoon (S)", "Maranoa (R)", "Mareeba (S)",
    "Central Darling (A)", "Moreton Bay (R)", "Mornington (S)", "Mount Isa (C)",
    "Murweh (S)", "Napranum (S)", "Noosa (S)", "North Burnett (R)",
    "Northern Peninsula Area (R)", "Palm Island (S)", "Paroo (S)",
    "Cessnock (C)", "Pormpuraaw (S)", "Quilpie (S)", "Redland (C)",
    "Richmond (S)", "Rockhampton (R)", "Scenic Rim (R)", "Somerset (R)",
    "South Burnett (R)", "Southern Downs (R)", "Sunshine Coast (R)",
    "Clarence Valley (A)", "Tablelands (R)", "Toowoomba (R)", "Torres (S)",
    "Torres Strait Island (R)", "Townsville (C)", "Weipa (T)", "Western Downs (R)",
    "Whitsunday (R)", "Winton (S)", "Woorabinda (S)", "Cobar (A)",
    "Wujal Wujal (S)", "Yarrabah (S)", "Adelaide (C)", "Adelaide Hills (DC)",
    "Alexandrina (DC)", "Anangu Pitjantjatjara (AC)", "Barossa (DC)",
    "Barunga West (DC)", "Balranald (A)", "Coffs Harbour (C)", "Berri and Barmera (DC)",
    "Burnside (C)", "Campbelltown (C) (SA)", "Ceduna (DC)", "Charles Sturt (C)",
    "Clare and Gilbert Valleys (DC)", "Cleve (DC)", "Coober Pedy (DC)",
    "Copper Coast (DC)", "Elliston (DC)", "Coolamon (A)", "Flinders Ranges (DC)",
    "Franklin Harbour (DC)", "Gawler (T)", "Goyder (DC)", "Grant (DC)",
    "Holdfast Bay (C)", "Kangaroo Island (DC)", "Karoonda East Murray (DC)",
    "Kimba (DC)", "Kingston (DC) (SA)", "Coonamble (A)", "Light (RegC)",
    "Lower Eyre Peninsula (DC)", "Loxton Waikerie (DC)", "Mallala (DC)",
    "Maralinga Tjarutja (AC)", "Marion (C)", "Mid Murray (DC)", "Mitcham (C)",
    "Mount Barker (DC)", "Mount Gambier (C)", "Cowra (A)", "Mount Remarkable (DC)",
    "Murray Bridge (RC)", "Naracoorte and Lucindale (DC)", "Northern Areas (DC)",
    "Norwood Payneham St Peters (C)", "Onkaparinga (C)", "Orroroo/Carrieton (DC)",
    "Peterborough (DC)", "Playford (C)", "Port Adelaide Enfield (C)",
    "Cumberland (A)", "Port Augusta (C)", "Port Lincoln (C)", "Port Pirie City and Dists (M)",
    "Prospect (C)", "Renmark Paringa (DC)", "Robe (DC)", "Roxby Downs (M)",
    "Salisbury (C)", "Southern Mallee (DC)", "Streaky Bay (DC)",
    "Dungog (A)", "Tatiara (DC)", "Tea Tree Gully (C)", "The Coorong (DC)",
    "Tumby Bay (DC)", "Unley (C)", "Victor Harbor (C)", "Wakefield (DC)",
    "Walkerville (M)", "Wattle Range (DC)", "West Torrens (C)", "Edward River (A)",
    "Whyalla (C)", "Wudinna (DC)", "Yankalilla (DC)", "Yorke Peninsula (DC)",
    "Unincorporated SA", "Albany (C)", "Armadale (C)", "Ashburton (S)",
    "Eurobodalla (A)", "Augusta-Margaret River (S)", "Bassendean (T)",
    "Bayswater (C)", "Belmont (C)", "Beverley (S)", "Boddington (S)",
    "Boyup Brook (S)", "Bridgetown-Greenbushes (S)", "Brookton (S)",
    "Broome (S)", "Fairfield (C)", "Broomehill-Tambellup (S)", "Bruce Rock (S)",
    "Bunbury (C)", "Busselton (C)", "Cambridge (T)", "Canning (C)",
    "Capel (S)", "Carnamah (S)", "Carnarvon (S)", "Chapman Valley (S)",
    "Federation (A)", "Chittering (S)", "Claremont (T)", "Cockburn (C)",
    "Collie (S)", "Coolgardie (S)", "Coorow (S)", "Corrigin (S)",
    "Cottesloe (T)", "Cranbrook (S)", "Cuballing (S)", "Bathurst Regional (A)",
    "Forbes (A)", "Cue (S)", "Cunderdin (S)", "Dalwallinu (S)", "Dandaragan (S)",
    "Dardanup (S)", "Denmark (S)", "Derby-West Kimberley (S)", "Donnybrook-Balingup (S)",
    "Dowerin (S)", "Dumbleyung (S)", "Georges River (A)", "Dundas (S)",
    "East Fremantle (T)", "East Pilbara (S)", "Esperance (S)", "Exmouth (S)",
    "Fremantle (C)", "Gingin (S)", "Gnowangerup (S)", "Goomalling (S)",
    "Gosnells (C)", "Gilgandra (A)", "Greater Geraldton (C)", "Halls Creek (S)",
    "Harvey (S)", "Irwin (S)", "Jerramungup (S)", "Joondalup (C)",
    "Kalamunda (S)", "Kalgoorlie/Boulder (C)", "Karratha (C)", "Katanning (S)",
    "Glen Innes Severn (A)", "Kellerberrin (S)", "Kent (S)", "Kojonup (S)",
    "Kondinin (S)", "Koorda (S)", "Kulin (S)", "Kwinana (C)", "Lake Grace (S)",
    "Laverton (S)", "Leonora (S)", "Goulburn Mulwaree (A)", "Mandurah (C)",
    "Manjimup (S)", "Meekatharra (S)", "Melville (C)", "Menzies (S)",
    "Merredin (S)", "Mingenew (S)", "Moora (S)", "Morawa (S)", "Mosman Park (T)",
    "Greater Hume Shire (A)", "Mount Magnet (S)", "Mount Marshall (S)",
    "Mukinbudin (S)", "Mundaring (S)", "Murchison (S)", "Murray (S)",
    "Nannup (S)", "Narembeen (S)", "Narrogin (S)", "Nedlands (C)",
    "Griffith (C)", "Ngaanyatjarraku (S)", "Northam (S)", "Northampton (S)",
    "Nungarin (S)", "Peppermint Grove (S)", "Perenjori (S)", "Perth (C)",
    "Pingelly (S)", "Plantagenet (S)", "Port Hedland (T)", "Gundagai (A)",
    "Quairading (S)", "Ravensthorpe (S)", "Rockingham (C)", "Sandstone (S)",
    "Serpentine-Jarrahdale (S)", "Shark Bay (S)", "South Perth (C)",
    "Stirling (C)", "Subiaco (C)", "Swan (C)", "Gunnedah (A)", "Tammin (S)",
    "Three Springs (S)", "Toodyay (S)", "Trayning (S)", "Upper Gascoyne (S)",
    "Victoria Park (T)", "Victoria Plains (S)", "Vincent (C)", "Wagin (S)",
    "Wandering (S)", "Gwydir (A)", "Wanneroo (C)", "Waroona (S)",
    "West Arthur (S)", "Westonia (S)", "Wickepin (S)", "Williams (S)",
    "Wiluna (S)", "Wongan-Ballidu (S)", "Woodanilling (S)", "Wyalkatchem (S)",
    "Bega Valley (A)", "Hawkesbury (C)", "Wyndham-East Kimberley (S)",
    "Yalgoo (S)", "Yilgarn (S)", "York (S)", "Break O'Day (M)", "Brighton (M)",
    "Burnie (C)", "Central Coast (M) (Tas.)", "Hay (A)", "Central Highlands (M) (Tas.)",
    "Circular Head (M)", "Clarence (C)", "Derwent Valley (M)", "Devonport (C)",
    "Dorset (M)", "Flinders (M) (Tas.)", "George Town (M)", "Glamorgan/Spring Bay (M)",
    "Glenorchy (C)", "Hilltops (A)", "Hobart (C)", "Huon Valley (M)",
    "Kentish (M)", "King Island (M)", "Kingborough (M)", "Latrobe (M) (Tas.)",
    "Launceston (C)", "Meander Valley (M)", "Northern Midlands (M)",
    "Sorell (M)", "Hornsby (A)", "Southern Midlands (M)", "Tasman (M)",
    "Waratah/Wynyard (M)", "West Coast (M)", "West Tamar (M)", "Alice Springs (T)",
    "Barkly (R)", "Belyuen (S)", "Hunters Hill (A)", "Central Desert (R)",
    "Coomalie (S)", "Darwin (C)", "East Arnhem (R)", "Katherine (T)",
    "Litchfield (M)", "MacDonnell (R)", "Palmerston (C)", "Roper Gulf (R)",
    "Tiwi Islands (R)", "Inner West (A)", "Victoria Daly (R)", "Wagait (S)",
    "West Arnhem (R)", "West Daly (R)", "Unincorporated NT", "Unincorporated ACT",
    "Inverell (A)", "Unincorp. Other Territories", "Junee (A)", "Kempsey (A)",
    "Kiama (A)", "Bellingen (A)", "Ku-ring-gai (A)", "Kyogle (A)",
    "Lachlan (A)", "Lake Macquarie (C)", "Lane Cove (A)", "Leeton (A)",
    "Lismore (C)", "Lithgow (C)", "Liverpool (C)", "Liverpool Plains (A)",
    "Berrigan (A)", "Lockhart (A)", "Maitland (C)", "Mid-Coast (A)",
    "Mid-Western Regional (A)", "Moree Plains (A)", "Mosman (A)",
    "Murray River (A)", "Murrumbidgee (A)", "Muswellbrook (A)", "Nambucca (A)",
    "Blacktown (C)", "Narrabri (A)", "Narrandera (A)", "Narromine (A)",
    "Newcastle (C)", "North Sydney (A)", "Northern Beaches (A)",
    "Oberon (A)", "Orange (C)", "Parkes (A)", "Parramatta (C)", "Bland (A)",
    "Penrith (C)", "Port Macquarie-Hastings (A)", "Port Stephens (A)",
    "Queanbeyan-Palerang Regional (A)", "Randwick (C)", "Richmond Valley (A)",
    "Rockdale (C)", "Ryde (C)", "Shellharbour (C)", "Shoalhaven (C)"
  )
}
