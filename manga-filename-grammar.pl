:- [library(dcg/basics)].

manga(Title, Lang, ChapterStart, ChapterEnd, SpecialStart, SpecialEnd,
Type, VolumeStart, VolumeEnd, Comment, Group, Revision) -->
	string(Title), " ",
	lang(Lang),
	"- ", 
	chapters(ChapterStart, ChapterEnd, SpecialStart, SpecialEnd), " ",
	source(Type, VolumeStart, VolumeEnd), " ",
	comment(Comment),
	group(Group),
	revision(Revision),
	".zip", !.

lang("eng") --> [].
lang("abk") --> "[abk] ".
lang("aar") --> "[aar] ".
lang("afr") --> "[afr] ".
lang("aka") --> "[aka] ".
lang("sqi") --> "[sqi] ".
lang("alb") --> "[alb] ".
lang("amh") --> "[amh] ".
lang("ara") --> "[ara] ".
lang("arg") --> "[arg] ".
lang("hye") --> "[hye] ".
lang("arm") --> "[arm] ".
lang("asm") --> "[asm] ".
lang("ava") --> "[ava] ".
lang("ave") --> "[ave] ".
lang("aym") --> "[aym] ".
lang("aze") --> "[aze] ".
lang("bam") --> "[bam] ".
lang("bak") --> "[bak] ".
lang("eus") --> "[eus] ".
lang("baq") --> "[baq] ".
lang("bel") --> "[bel] ".
lang("ben") --> "[ben] ".
lang("bih") --> "[bih] ".
lang("bis") --> "[bis] ".
lang("bos") --> "[bos] ".
lang("bre") --> "[bre] ".
lang("bul") --> "[bul] ".
lang("mya") --> "[mya] ".
lang("bur") --> "[bur] ".
lang("cat") --> "[cat] ".
lang("cha") --> "[cha] ".
lang("che") --> "[che] ".
lang("nya") --> "[nya] ".
lang("zho") --> "[zho] ".
lang("chi") --> "[chi] ".
lang("chv") --> "[chv] ".
lang("cor") --> "[cor] ".
lang("cos") --> "[cos] ".
lang("cre") --> "[cre] ".
lang("hrv") --> "[hrv] ".
lang("ces") --> "[ces] ".
lang("cze") --> "[cze] ".
lang("dan") --> "[dan] ".
lang("div") --> "[div] ".
lang("nld") --> "[nld] ".
lang("dut") --> "[dut] ".
lang("dzo") --> "[dzo] ".
lang("epo") --> "[epo] ".
lang("est") --> "[est] ".
lang("ewe") --> "[ewe] ".
lang("fao") --> "[fao] ".
lang("fij") --> "[fij] ".
lang("fin") --> "[fin] ".
lang("fra") --> "[fra] ".
lang("fre") --> "[fre] ".
lang("ful") --> "[ful] ".
lang("glg") --> "[glg] ".
lang("kat") --> "[kat] ".
lang("geo") --> "[geo] ".
lang("deu") --> "[deu] ".
lang("ger") --> "[ger] ".
lang("ell") --> "[ell] ".
lang("gre") --> "[gre] ".
lang("grn") --> "[grn] ".
lang("guj") --> "[guj] ".
lang("hat") --> "[hat] ".
lang("hau") --> "[hau] ".
lang("heb") --> "[heb] ".
lang("her") --> "[her] ".
lang("hin") --> "[hin] ".
lang("hmo") --> "[hmo] ".
lang("hun") --> "[hun] ".
lang("ina") --> "[ina] ".
lang("ind") --> "[ind] ".
lang("ile") --> "[ile] ".
lang("gle") --> "[gle] ".
lang("ibo") --> "[ibo] ".
lang("ipk") --> "[ipk] ".
lang("ido") --> "[ido] ".
lang("isl") --> "[isl] ".
lang("ice") --> "[ice] ".
lang("ita") --> "[ita] ".
lang("iku") --> "[iku] ".
lang("jpn") --> "[jpn] ".
lang("jav") --> "[jav] ".
lang("kal") --> "[kal] ".
lang("kan") --> "[kan] ".
lang("kau") --> "[kau] ".
lang("kas") --> "[kas] ".
lang("kaz") --> "[kaz] ".
lang("khm") --> "[khm] ".
lang("kik") --> "[kik] ".
lang("kin") --> "[kin] ".
lang("kir") --> "[kir] ".
lang("kom") --> "[kom] ".
lang("kon") --> "[kon] ".
lang("kor") --> "[kor] ".
lang("kur") --> "[kur] ".
lang("kua") --> "[kua] ".
lang("lat") --> "[lat] ".
lang("ltz") --> "[ltz] ".
lang("lug") --> "[lug] ".
lang("lim") --> "[lim] ".
lang("lin") --> "[lin] ".
lang("lao") --> "[lao] ".
lang("lit") --> "[lit] ".
lang("lub") --> "[lub] ".
lang("lav") --> "[lav] ".
lang("glv") --> "[glv] ".
lang("mkd") --> "[mkd] ".
lang("mac") --> "[mac] ".
lang("mlg") --> "[mlg] ".
lang("msa") --> "[msa] ".
lang("may") --> "[may] ".
lang("mal") --> "[mal] ".
lang("mlt") --> "[mlt] ".
lang("mri") --> "[mri] ".
lang("mao") --> "[mao] ".
lang("mar") --> "[mar] ".
lang("mah") --> "[mah] ".
lang("mon") --> "[mon] ".
lang("nau") --> "[nau] ".
lang("nav") --> "[nav] ".
lang("nde") --> "[nde] ".
lang("nep") --> "[nep] ".
lang("ndo") --> "[ndo] ".
lang("nob") --> "[nob] ".
lang("nno") --> "[nno] ".
lang("nor") --> "[nor] ".
lang("iii") --> "[iii] ".
lang("nbl") --> "[nbl] ".
lang("oci") --> "[oci] ".
lang("oji") --> "[oji] ".
lang("chu") --> "[chu] ".
lang("orm") --> "[orm] ".
lang("ori") --> "[ori] ".
lang("oss") --> "[oss] ".
lang("pan") --> "[pan] ".
lang("pli") --> "[pli] ".
lang("fas") --> "[fas] ".
lang("per") --> "[per] ".
lang("pol") --> "[pol] ".
lang("pus") --> "[pus] ".
lang("por") --> "[por] ".
lang("que") --> "[que] ".
lang("roh") --> "[roh] ".
lang("run") --> "[run] ".
lang("ron") --> "[ron] ".
lang("rum") --> "[rum] ".
lang("rus") --> "[rus] ".
lang("san") --> "[san] ".
lang("srd") --> "[srd] ".
lang("snd") --> "[snd] ".
lang("sme") --> "[sme] ".
lang("smo") --> "[smo] ".
lang("sag") --> "[sag] ".
lang("srp") --> "[srp] ".
lang("gla") --> "[gla] ".
lang("sna") --> "[sna] ".
lang("sin") --> "[sin] ".
lang("slk") --> "[slk] ".
lang("slo") --> "[slo] ".
lang("slv") --> "[slv] ".
lang("som") --> "[som] ".
lang("sot") --> "[sot] ".
lang("spa") --> "[spa] ".
lang("sun") --> "[sun] ".
lang("swa") --> "[swa] ".
lang("ssw") --> "[ssw] ".
lang("swe") --> "[swe] ".
lang("tam") --> "[tam] ".
lang("tel") --> "[tel] ".
lang("tgk") --> "[tgk] ".
lang("tha") --> "[tha] ".
lang("tir") --> "[tir] ".
lang("bod") --> "[bod] ".
lang("tib") --> "[tib] ".
lang("tuk") --> "[tuk] ".
lang("tgl") --> "[tgl] ".
lang("tsn") --> "[tsn] ".
lang("ton") --> "[ton] ".
lang("tur") --> "[tur] ".
lang("tso") --> "[tso] ".
lang("tat") --> "[tat] ".
lang("twi") --> "[twi] ".
lang("tah") --> "[tah] ".
lang("uig") --> "[uig] ".
lang("ukr") --> "[ukr] ".
lang("urd") --> "[urd] ".
lang("uzb") --> "[uzb] ".
lang("ven") --> "[ven] ".
lang("vie") --> "[vie] ".
lang("vol") --> "[vol] ".
lang("wln") --> "[wln] ".
lang("cym") --> "[cym] ".
lang("wel") --> "[wel] ".
lang("wol") --> "[wol] ".
lang("fry") --> "[fry] ".
lang("xho") --> "[xho] ".
lang("yid") --> "[yid] ".
lang("yor") --> "[yor] ".
lang("zha") --> "[zha] ".
lang("zul") --> "[zul] ".

two_digits([A, B]) --> digit(A), digit(B).
three_digits([A, B, C]) --> digit(A), digit(B), digit(C).
four_digits([A, B, C, D]) --> digit(A), digit(B), digit(C), digit(D).

chapters("", "", "", "") --> "000".
chapters(C_Start, "", "", "") --> "c", three_digits(C_Start).
chapters(C_Start, "", S_Start, "") --> "c", three_digits(C_Start), "x", digit(S_Start).
chapters(C_Start, "", S_Start, "") --> "c", three_digits(C_Start), "y", two_digits(S_Start).
chapters(C_Start, "", S_Start, "") --> "c", three_digits(C_Start), "z", three_digits(S_Start).
chapters(C_Start, C_End, "", "") --> three_digits(C_Start), "-", three_digits(C_End).
chapters(C_Start, C_End, "", "") --> "c", three_digits(C_Start), "-", three_digits(C_End).
chapters(C_Start, C_End, S_Start, "") --> "c", three_digits(C_Start), "-", three_digits(C_End), "x", digit(S_Start).
chapters(C_Start, C_End, S_Start, S_End) --> "c", three_digits(C_Start), "-", three_digits(C_End), "x", digit(S_Start), "-", digit(S_End).
chapters(C_Start, C_End, S_Start, "") --> "c", three_digits(C_Start), "-", three_digits(C_End), "y", two_digits(S_Start).
chapters(C_Start, C_End, S_Start, S_End) --> "c", three_digits(C_Start), "-", three_digits(C_End), "y", two_digits(S_Start), "-", two_digits(S_End).
chapters(C_Start, C_End, S_Start, "") --> "c", three_digits(C_Start), "-", three_digits(C_End), "z", three_digits(S_Start).
chapters(C_Start, C_End, S_Start, S_End) --> "c", three_digits(C_Start), "-", three_digits(C_End), "z", three_digits(S_Start), "-", three_digits(S_End).

chapters(C_Start, "", "", "") --> "d", four_digits(C_Start).
chapters(C_Start, "", S_Start, "") --> "d", four_digits(C_Start), "x", digit(S_Start).
chapters(C_Start, "", S_Start, "") --> "d", four_digits(C_Start), "y", two_digits(S_Start).
chapters(C_Start, "", S_Start, "") --> "d", four_digits(C_Start), "z", three_digits(S_Start).
chapters(C_Start, C_End, "", "") --> four_digits(C_Start), "-", four_digits(C_End).
chapters(C_Start, C_End, "", "") --> "d", four_digits(C_Start), "-", four_digits(C_End).
chapters(C_Start, C_End, S_Start, "") --> "d", four_digits(C_Start), "-", four_digits(C_End), "x", digit(S_Start).
chapters(C_Start, C_End, S_Start, S_End) --> "d", four_digits(C_Start), "-", four_digits(C_End), "x", digit(S_Start), "-", digit(S_End).
chapters(C_Start, C_End, S_Start, "") --> "d", four_digits(C_Start), "-", four_digits(C_End), "y", two_digits(S_Start).
chapters(C_Start, C_End, S_Start, S_End) --> "d", four_digits(C_Start), "-", four_digits(C_End), "y", two_digits(S_Start), "-", two_digits(S_End).
chapters(C_Start, C_End, S_Start, "") --> "d", four_digits(C_Start), "-", four_digits(C_End), "z", three_digits(S_Start).
chapters(C_Start, C_End, S_Start, S_End) --> "d", four_digits(C_Start), "-", four_digits(C_End), "z", three_digits(S_Start), "-", three_digits(S_End).

source("web", "", "") --> "(web)".
source("mag", "", "") --> "(mag)".
source("mix", "", "") --> "(mix)".
source("", Start, "") --> "(v", two_digits(Start), ")".
source("", Start, End) --> "(v", two_digits(Start), "-", two_digits(End), ")".
source("", Start, End) --> "(v", two_digits(Start), "-w", three_digits(End), ")".
source("", Start, "") --> "(w", three_digits(Start), ")".
source("", Start, End) --> "(w", three_digits(Start), "-", three_digits(End), ")".

comment("") --> [].
comment(C) --> "[", string(C), "] ".

group("Unknown") --> "[Unknown]".
group(G) --> "[", string(G), "]".

revision("") --> [].
revision(R) --> "{v", digit(R), "}".
revision(R) --> "{r", digit(R), "}".
revision(R) --> "{", digit(R), "}".
