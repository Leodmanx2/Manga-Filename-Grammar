:- [library(dcg/basics)].

manga(Title, Lang, ChapterStart, ChapterEnd, SpecialStart, SpecialEnd,
Type, VolumeStart, VolumeEnd, Comment, Group, Revision) -->
	string(Title),
	lang(Lang),
	"-",
	chapters(ChapterStart, ChapterEnd, SpecialStart, SpecialEnd),
	source(Type, VolumeStart, VolumeEnd),
	comment(Comment),
	group(Group),
	revision(Revision),
	".zip".

lang("eng") --> whites.
lang("abk") --> whites, "[abk]", whites.
lang("aar") --> whites, "[aar]", whites.
lang("afr") --> whites, "[afr]", whites.
lang("aka") --> whites, "[aka]", whites.
lang("sqi") --> whites, "[sqi]", whites.
lang("alb") --> whites, "[alb]", whites.
lang("amh") --> whites, "[amh]", whites.
lang("ara") --> whites, "[ara]", whites.
lang("arg") --> whites, "[arg]", whites.
lang("hye") --> whites, "[hye]", whites.
lang("arm") --> whites, "[arm]", whites.
lang("asm") --> whites, "[asm]", whites.
lang("ava") --> whites, "[ava]", whites.
lang("ave") --> whites, "[ave]", whites.
lang("aym") --> whites, "[aym]", whites.
lang("aze") --> whites, "[aze]", whites.
lang("bam") --> whites, "[bam]", whites.
lang("bak") --> whites, "[bak]", whites.
lang("eus") --> whites, "[eus]", whites.
lang("baq") --> whites, "[baq]", whites.
lang("bel") --> whites, "[bel]", whites.
lang("ben") --> whites, "[ben]", whites.
lang("bih") --> whites, "[bih]", whites.
lang("bis") --> whites, "[bis]", whites.
lang("bos") --> whites, "[bos]", whites.
lang("bre") --> whites, "[bre]", whites.
lang("bul") --> whites, "[bul]", whites.
lang("mya") --> whites, "[mya]", whites.
lang("bur") --> whites, "[bur]", whites.
lang("cat") --> whites, "[cat]", whites.
lang("cha") --> whites, "[cha]", whites.
lang("che") --> whites, "[che]", whites.
lang("nya") --> whites, "[nya]", whites.
lang("zho") --> whites, "[zho]", whites.
lang("chi") --> whites, "[chi]", whites.
lang("chv") --> whites, "[chv]", whites.
lang("cor") --> whites, "[cor]", whites.
lang("cos") --> whites, "[cos]", whites.
lang("cre") --> whites, "[cre]", whites.
lang("hrv") --> whites, "[hrv]", whites.
lang("ces") --> whites, "[ces]", whites.
lang("cze") --> whites, "[cze]", whites.
lang("dan") --> whites, "[dan]", whites.
lang("div") --> whites, "[div]", whites.
lang("nld") --> whites, "[nld]", whites.
lang("dut") --> whites, "[dut]", whites.
lang("dzo") --> whites, "[dzo]", whites.
lang("epo") --> whites, "[epo]", whites.
lang("est") --> whites, "[est]", whites.
lang("ewe") --> whites, "[ewe]", whites.
lang("fao") --> whites, "[fao]", whites.
lang("fij") --> whites, "[fij]", whites.
lang("fin") --> whites, "[fin]", whites.
lang("fra") --> whites, "[fra]", whites.
lang("fre") --> whites, "[fre]", whites.
lang("ful") --> whites, "[ful]", whites.
lang("glg") --> whites, "[glg]", whites.
lang("kat") --> whites, "[kat]", whites.
lang("geo") --> whites, "[geo]", whites.
lang("deu") --> whites, "[deu]", whites.
lang("ger") --> whites, "[ger]", whites.
lang("ell") --> whites, "[ell]", whites.
lang("gre") --> whites, "[gre]", whites.
lang("grn") --> whites, "[grn]", whites.
lang("guj") --> whites, "[guj]", whites.
lang("hat") --> whites, "[hat]", whites.
lang("hau") --> whites, "[hau]", whites.
lang("heb") --> whites, "[heb]", whites.
lang("her") --> whites, "[her]", whites.
lang("hin") --> whites, "[hin]", whites.
lang("hmo") --> whites, "[hmo]", whites.
lang("hun") --> whites, "[hun]", whites.
lang("ina") --> whites, "[ina]", whites.
lang("ind") --> whites, "[ind]", whites.
lang("ile") --> whites, "[ile]", whites.
lang("gle") --> whites, "[gle]", whites.
lang("ibo") --> whites, "[ibo]", whites.
lang("ipk") --> whites, "[ipk]", whites.
lang("ido") --> whites, "[ido]", whites.
lang("isl") --> whites, "[isl]", whites.
lang("ice") --> whites, "[ice]", whites.
lang("ita") --> whites, "[ita]", whites.
lang("iku") --> whites, "[iku]", whites.
lang("jpn") --> whites, "[jpn]", whites.
lang("jav") --> whites, "[jav]", whites.
lang("kal") --> whites, "[kal]", whites.
lang("kan") --> whites, "[kan]", whites.
lang("kau") --> whites, "[kau]", whites.
lang("kas") --> whites, "[kas]", whites.
lang("kaz") --> whites, "[kaz]", whites.
lang("khm") --> whites, "[khm]", whites.
lang("kik") --> whites, "[kik]", whites.
lang("kin") --> whites, "[kin]", whites.
lang("kir") --> whites, "[kir]", whites.
lang("kom") --> whites, "[kom]", whites.
lang("kon") --> whites, "[kon]", whites.
lang("kor") --> whites, "[kor]", whites.
lang("kur") --> whites, "[kur]", whites.
lang("kua") --> whites, "[kua]", whites.
lang("lat") --> whites, "[lat]", whites.
lang("ltz") --> whites, "[ltz]", whites.
lang("lug") --> whites, "[lug]", whites.
lang("lim") --> whites, "[lim]", whites.
lang("lin") --> whites, "[lin]", whites.
lang("lao") --> whites, "[lao]", whites.
lang("lit") --> whites, "[lit]", whites.
lang("lub") --> whites, "[lub]", whites.
lang("lav") --> whites, "[lav]", whites.
lang("glv") --> whites, "[glv]", whites.
lang("mkd") --> whites, "[mkd]", whites.
lang("mac") --> whites, "[mac]", whites.
lang("mlg") --> whites, "[mlg]", whites.
lang("msa") --> whites, "[msa]", whites.
lang("may") --> whites, "[may]", whites.
lang("mal") --> whites, "[mal]", whites.
lang("mlt") --> whites, "[mlt]", whites.
lang("mri") --> whites, "[mri]", whites.
lang("mao") --> whites, "[mao]", whites.
lang("mar") --> whites, "[mar]", whites.
lang("mah") --> whites, "[mah]", whites.
lang("mon") --> whites, "[mon]", whites.
lang("nau") --> whites, "[nau]", whites.
lang("nav") --> whites, "[nav]", whites.
lang("nde") --> whites, "[nde]", whites.
lang("nep") --> whites, "[nep]", whites.
lang("ndo") --> whites, "[ndo]", whites.
lang("nob") --> whites, "[nob]", whites.
lang("nno") --> whites, "[nno]", whites.
lang("nor") --> whites, "[nor]", whites.
lang("iii") --> whites, "[iii]", whites.
lang("nbl") --> whites, "[nbl]", whites.
lang("oci") --> whites, "[oci]", whites.
lang("oji") --> whites, "[oji]", whites.
lang("chu") --> whites, "[chu]", whites.
lang("orm") --> whites, "[orm]", whites.
lang("ori") --> whites, "[ori]", whites.
lang("oss") --> whites, "[oss]", whites.
lang("pan") --> whites, "[pan]", whites.
lang("pli") --> whites, "[pli]", whites.
lang("fas") --> whites, "[fas]", whites.
lang("per") --> whites, "[per]", whites.
lang("pol") --> whites, "[pol]", whites.
lang("pus") --> whites, "[pus]", whites.
lang("por") --> whites, "[por]", whites.
lang("que") --> whites, "[que]", whites.
lang("roh") --> whites, "[roh]", whites.
lang("run") --> whites, "[run]", whites.
lang("ron") --> whites, "[ron]", whites.
lang("rum") --> whites, "[rum]", whites.
lang("rus") --> whites, "[rus]", whites.
lang("san") --> whites, "[san]", whites.
lang("srd") --> whites, "[srd]", whites.
lang("snd") --> whites, "[snd]", whites.
lang("sme") --> whites, "[sme]", whites.
lang("smo") --> whites, "[smo]", whites.
lang("sag") --> whites, "[sag]", whites.
lang("srp") --> whites, "[srp]", whites.
lang("gla") --> whites, "[gla]", whites.
lang("sna") --> whites, "[sna]", whites.
lang("sin") --> whites, "[sin]", whites.
lang("slk") --> whites, "[slk]", whites.
lang("slo") --> whites, "[slo]", whites.
lang("slv") --> whites, "[slv]", whites.
lang("som") --> whites, "[som]", whites.
lang("sot") --> whites, "[sot]", whites.
lang("spa") --> whites, "[spa]", whites.
lang("sun") --> whites, "[sun]", whites.
lang("swa") --> whites, "[swa]", whites.
lang("ssw") --> whites, "[ssw]", whites.
lang("swe") --> whites, "[swe]", whites.
lang("tam") --> whites, "[tam]", whites.
lang("tel") --> whites, "[tel]", whites.
lang("tgk") --> whites, "[tgk]", whites.
lang("tha") --> whites, "[tha]", whites.
lang("tir") --> whites, "[tir]", whites.
lang("bod") --> whites, "[bod]", whites.
lang("tib") --> whites, "[tib]", whites.
lang("tuk") --> whites, "[tuk]", whites.
lang("tgl") --> whites, "[tgl]", whites.
lang("tsn") --> whites, "[tsn]", whites.
lang("ton") --> whites, "[ton]", whites.
lang("tur") --> whites, "[tur]", whites.
lang("tso") --> whites, "[tso]", whites.
lang("tat") --> whites, "[tat]", whites.
lang("twi") --> whites, "[twi]", whites.
lang("tah") --> whites, "[tah]", whites.
lang("uig") --> whites, "[uig]", whites.
lang("ukr") --> whites, "[ukr]", whites.
lang("urd") --> whites, "[urd]", whites.
lang("uzb") --> whites, "[uzb]", whites.
lang("ven") --> whites, "[ven]", whites.
lang("vie") --> whites, "[vie]", whites.
lang("vol") --> whites, "[vol]", whites.
lang("wln") --> whites, "[wln]", whites.
lang("cym") --> whites, "[cym]", whites.
lang("wel") --> whites, "[wel]", whites.
lang("wol") --> whites, "[wol]", whites.
lang("fry") --> whites, "[fry]", whites.
lang("xho") --> whites, "[xho]", whites.
lang("yid") --> whites, "[yid]", whites.
lang("yor") --> whites, "[yor]", whites.
lang("zha") --> whites, "[zha]", whites.
lang("zul") --> whites, "[zul]", whites.

two_digits([A, B]) --> digit(A), digit(B).
three_digits([A, B, C]) --> digit(A), digit(B), digit(C).
four_digits([A, B, C, D]) --> digit(A), digit(B), digit(C), digit(D).

chapters("", "", "", "") --> whites, "000 ".
chapters(C_Start, "", "", "") --> whites, "c", three_digits(C_Start), whites.
chapters(C_Start, "", S_Start, "") --> whites, "c", three_digits(C_Start), "x", digit(S_Start), whites.
chapters(C_Start, "", S_Start, "") --> whites, "c", three_digits(C_Start), "y", two_digits(S_Start), whites.
chapters(C_Start, "", S_Start, "") --> whites, "c", three_digits(C_Start), "z", three_digits(S_Start), whites.
chapters(C_Start, C_End, "", "") --> whites, three_digits(C_Start), "-", three_digits(C_End), whites.
chapters(C_Start, C_End, "", "") --> whites, "c", three_digits(C_Start), "-", three_digits(C_End), whites.
chapters(C_Start, C_End, S_Start, "") --> whites, "c", three_digits(C_Start), "-", three_digits(C_End), "x", digit(S_Start), whites.
chapters(C_Start, C_End, S_Start, S_End) --> whites, "c", three_digits(C_Start), "-", three_digits(C_End), "x", digit(S_Start), "-", digit(S_End), whites.
chapters(C_Start, C_End, S_Start, "") --> whites, "c", three_digits(C_Start), "-", three_digits(C_End), "y", two_digits(S_Start), whites.
chapters(C_Start, C_End, S_Start, S_End) --> whites, "c", three_digits(C_Start), "-", three_digits(C_End), "y", two_digits(S_Start), "-", two_digits(S_End), whites.
chapters(C_Start, C_End, S_Start, "") --> whites, "c", three_digits(C_Start), "-", three_digits(C_End), "z", three_digits(S_Start), whites.
chapters(C_Start, C_End, S_Start, S_End) --> whites, "c", three_digits(C_Start), "-", three_digits(C_End), "z", three_digits(S_Start), "-", three_digits(S_End), whites.

chapters(C_Start, "", "", "") --> whites, "d", four_digits(C_Start).
chapters(C_Start, "", S_Start, "") --> whites, "d", four_digits(C_Start), "x", digit(S_Start), whites.
chapters(C_Start, "", S_Start, "") --> whites, "d", four_digits(C_Start), "y", two_digits(S_Start), whites.
chapters(C_Start, "", S_Start, "") --> whites, "d", four_digits(C_Start), "z", three_digits(S_Start), whites.
chapters(C_Start, C_End, "", "") --> whites, four_digits(C_Start), "-", four_digits(C_End), whites.
chapters(C_Start, C_End, "", "") --> whites, "d", four_digits(C_Start), "-", four_digits(C_End), whites.
chapters(C_Start, C_End, S_Start, "") --> whites, "d", four_digits(C_Start), "-", four_digits(C_End), "x", digit(S_Start), whites.
chapters(C_Start, C_End, S_Start, S_End) --> whites, "d", four_digits(C_Start), "-", four_digits(C_End), "x", digit(S_Start), "-", digit(S_End), whites.
chapters(C_Start, C_End, S_Start, "") --> whites, "d", four_digits(C_Start), "-", four_digits(C_End), "y", two_digits(S_Start), whites.
chapters(C_Start, C_End, S_Start, S_End) --> whites, "d", four_digits(C_Start), "-", four_digits(C_End), "y", two_digits(S_Start), "-", two_digits(S_End), whites.
chapters(C_Start, C_End, S_Start, "") --> whites, "d", four_digits(C_Start), "-", four_digits(C_End), "z", three_digits(S_Start), whites.
chapters(C_Start, C_End, S_Start, S_End) --> whites, "d", four_digits(C_Start), "-", four_digits(C_End), "z", three_digits(S_Start), "-", three_digits(S_End), whites.

source("mag", "", "") --> whites, "(mag) ".
source("web", "", "") --> whites, "(web) ".
source("mix", "", "") --> whites, "(mix) ".
source("", Start, "") --> whites, "(v", two_digits(Start), ") ".
source("", Start, End) --> whites, "(v", two_digits(Start), "-", two_digits(End), ") ".
source("", Start, End) --> whites, "(v", two_digits(Start), "-w", three_digits(End), ") ".
source("", Start, "") --> whites, "(w", three_digits(Start), ") ".
source("", Start, End) --> whites, "(w", three_digits(Start), "-", three_digits(End), ") ".

comment(C) --> whites, "[", string(C), "]", whites.
comment("") --> whites.

group(G) --> whites, "[", string(G), "]", whites.

revision(R) --> "{v", digit(R), "}".
revision(R) --> "{r", digit(R), "}".
revision(R) --> "{", digit(R), "}".
revision("") --> whites.
