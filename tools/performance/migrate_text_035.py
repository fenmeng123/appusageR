"""Token-aware migration; strings/comments are left intact and every call is inventoried."""
from pathlib import Path
import csv,re
from implement_structures_035 import read,write,ROOT

BASE={'paste':'appusage_text_paste','paste0':'appusage_text_paste0','trimws':'appusage_text_trim',
 'strsplit':'appusage_text_split','grepl':'appusage_text_grepl','grep':'appusage_text_grep',
 'sub':'appusage_text_sub','gsub':'appusage_text_gsub','tolower':'appusage_text_lower','toupper':'appusage_text_upper',
 'nzchar':'appusage_text_nzchar','nchar':'appusage_text_nchar','substr':'appusage_text_substr','substring':'appusage_text_substring',
 'startsWith':'appusage_text_starts','endsWith':'appusage_text_ends','regexec':'appusage_text_regexec',
 'regexpr':'appusage_text_regexpr','gregexpr':'appusage_text_gregexpr','regmatches':'appusage_text_regmatches',
 'enc2utf8':'stringi::stri_enc_toutf8'}
STRINGR={'str_trim':'stringi::stri_trim_both','str_detect':'appusage_text_detect',
 'str_extract':'stringi::stri_extract_first_regex','str_remove':'appusage_text_remove',
 'str_replace':'appusage_text_str_replace','str_replace_all':'appusage_text_str_replace_all',
 'str_to_lower':'appusage_text_lower','str_to_upper':'appusage_text_upper'}

def main():
    entries=[]
    for path in sorted((ROOT/'R').glob('*.R')):
        if path.name=='text_engine.R': continue
        text=path.read_text(encoding='utf-8')
        pieces=[]; i=0
        while i<len(text):
            c=text[i]
            if c=='#':
                end=text.find('\n',i)
                if end<0: end=len(text)
                pieces.append(text[i:end]); i=end; continue
            if c in '\"\'`':
                end=i+1; escaped=False
                while end<len(text):
                    ch=text[end]
                    if escaped: escaped=False
                    elif ch=='\\': escaped=True
                    elif ch==c: end+=1; break
                    end+=1
                pieces.append(text[i:end]); i=end; continue
            match=re.match(r'[A-Za-z_.][A-Za-z0-9_.]*',text[i:])
            if match:
                token=match[0]; end=i+len(token); new=token
                if token=='stringr' and text[end:end+2]=='::':
                    fn=re.match(r'[A-Za-z_]+',text[end+2:])[0]
                    if fn not in STRINGR: raise ValueError(fn)
                    new=STRINGR[fn]; end+=2+len(fn)
                elif token in BASE and (not i or text[i-1] not in '$@:'):
                    # Named list/data fields (e.g. sub=...) are not function calls.
                    after=text[end:].lstrip()
                    if not after.startswith('='): new=BASE[token]
                if new!=token:
                    entries.append(dict(file='R/'+path.name,line=text.count('\n',0,i)+1,old=text[i:end],new=new))
                pieces.append(new); i=end; continue
            pieces.append(c); i+=1
        path.write_text(''.join(pieces),encoding='utf-8')
    text=read('utils.R')
    text=text.replace('converted <- iconv(x, from = encoding, to = "UTF-8", sub = "")','converted <- appusage_text_encode_skip(x, from = encoding)')
    text=text.replace('available_encodings = iconvlist(),\n                            converter = iconv','available_encodings = appusage_text_encoding_names(),\n                            converter = appusage_text_encode_strict')
    text=text.replace('"iconv returned NA"','"ICU strict conversion returned NA"')
    write('utils.R',text)
    desc=(ROOT/'DESCRIPTION').read_text(encoding='utf-8').replace('    stringr,\n','')
    (ROOT/'DESCRIPTION').write_text(desc,encoding='utf-8')
    with (ROOT.parent/'docs/reviews/TEXT_MIGRATION_0.3.5.csv').open('w',newline='',encoding='utf-8') as out:
        writer=csv.DictWriter(out,fieldnames=['file','line','old','new']); writer.writeheader();writer.writerows(entries)
    print('Migrated',len(entries),'data/log text references; encoding adapter replacements also applied')

if __name__=='__main__': main()
