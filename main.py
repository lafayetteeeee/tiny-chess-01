import json,os,urllib.request as R
V=[0,100,320,330,500,900,20000]
S="<<<<<<<<UUUUUUUUAAFKKFAA>>AHHA>><<<FF<<<>:7<<7:>>AA22AA><<<<<<<<#(----(#(2<<<<2(-<ADDA<-->DFFD>--<DFFD<-->ADDA>-(2<>><2(#(----(#-((##((--((##((--((##((--((##((-2--((--272222227FF<<<<FFFKA<<AKF"
T=[None]+[[(ord(c)-60)*2 for c in S[j*64:(j+1)*64]]for j in(0,1,1,0,1,2)]
SQ=[s for s in range(128)if~s&0x88]
KN=(-33,-31,-18,-14,14,18,31,33)
K8=(-17,-16,-15,-1,1,15,16,17)
DR=[0,0,KN,(-17,-15,15,17),(-16,-1,1,16),K8,K8]
CM={119:14,112:13,7:11,0:7,116:12,4:3}
def parse(fen):
 w=fen.split();b=[0]*128;sq=0
 for ch in w[0]:
  if ch=="/":sq+=8
  elif ch.isdigit():sq+=int(ch)
  else:b[sq]=("pnbrqk".index(ch.lower())+1)*(2*ch.isupper()-1);sq+=1
 ca=sum(1<<"KQkq".index(c)for c in w[2]if c!="-")
 return[b,2*(w[1]=="w")-1,ca,-1 if w[3]=="-"else(8-int(w[3][1]))*16+ord(w[3][0])-97]
def atk(p,sq,by):
 b=p[0]
 for ds,pv in(((15*by,17*by),1),(KN,2),(K8,6)):
  for d in ds:
   t=sq+d
   if not t&0x88 and b[t]==pv*by:return True
 for d in K8:
  pv=(4,3)[abs(d)in(15,17)];t=sq+d
  while not t&0x88:
   v=b[t]
   if v:
    if(v>0)==(by>0)and abs(v)in(pv,5):return True
    break
   t+=d
 return False
def gen(p,cp=False):
 b=p[0];us=p[1];w=us<0;o=[];ap=o.append
 for sq in SQ:
  v=b[sq]
  if not v or(v>0)==w:continue
  a=abs(v)
  if a==1:
   fw=-16*us;pr=7*w
   def pm(t):
    for x in(5,4,3,2):ap(sq|t<<7|x<<14)
   for d in(fw-1,fw,fw+1):
    t=sq+d
    if t&0x88:continue
    tv=b[t]
    if d==fw:
     if cp or tv:continue
     if(t>>4)==pr:pm(t)
     else:
      ap(sq|t<<7)
      if(sq>>4)==6-5*w and not b[t+fw]:ap(sq|(t+fw)<<7|1<<17)
    elif tv and(tv>0)==w:
     if(t>>4)==pr:pm(t)
     else:ap(sq|t<<7)
    elif t==p[3]:ap(sq|t<<7|2<<17)
  else:
   for d in DR[a]:
    t=sq+d
    while not t&0x88:
     tv=b[t]
     if tv:
      if(tv>0)==w:ap(sq|t<<7)
      break
     if not cp:ap(sq|t<<7)
     if a in(2,6):break
     t+=d
   if a==6 and not cp:
    for sd in(1,-1):
     bit=1<<(2*w+(sd<0));kt=sq+2*sd;rf=sq+sd*(4-(sd>0))
     if p[2]&bit and b[rf]==4*us and not b[sq+sd]and not b[kt]and(sd>0 or not b[sq+3*sd])and not any(atk(p,x,-us)for x in(sq,sq+sd,kt)):ap(sq|kt<<7|(4-(sd>0))<<17)
 return o
def mk(p,m):
 b=p[0][:];f=m&127;t=m>>7&127;pr=m>>14&7;fl=m>>17;v=b[f];us=2*(v>0)-1
 if fl==2:b[t+16*us]=0
 b[t]=v;b[f]=0
 if pr:b[t]=pr*us
 if fl>=3:
  rf,rt=(f+3,f+1)if fl==3 else(f-4,f-1);b[rt]=b[rf];b[rf]=0
 return[b,-p[1],p[2]&CM.get(f,15)&CM.get(t,15),f-16*us if fl==1 else-1]
TT={};N=ST=0;HS=set()
def sr(p,al,be,d):
 global N,ST
 N+=1
 if ST or N>12e4:ST=1;return 0
 k=(tuple(p[0]),*p[1:])
 if k in HS:return 0
 HS.add(k)
 b=p[0];chk=atk(p,b.index(6*p[1]),-p[1]);tm=0
 if d>0:tm=TT.get(k,0)
 elif not chk:
  st=0
  for sq,v in enumerate(b):
   if not v:continue
   a=abs(v);sg=2*(v>0)-1;i=(sq>>4)*8+(sq&7)
   st+=sg*(V[a]+T[a][i if sg>0 else i^56])
  st*=p[1]
  if st>=be:HS.discard(k);return be
  if st>al:al=st
 sc=[]
 for m in gen(p,d<=0 and not chk):
  cv=abs(b[m>>7&127]);v=0
  if cv or m>>17==2:v=V[cv if cv else 1]*16-V[abs(b[m&127])]
  if m==tm:v=1<<28
  sc.append((v,m))
 sc.sort(reverse=True);best=-200000;bm=0
 for _,m in sc:
  if abs(b[m>>7&127])==6:HS.discard(k);return 300000
  v=-sr(mk(p,m),-be,-al,d-1)
  if v<-2e5:continue
  if v>best:best=v;bm=m
  if v>al:al=v
  if al>=be:break
 if not bm:HS.discard(k);return-100000 if chk else(al if d<=0 else 0)
 if d>0 and not ST:TT[k]=bm
 HS.discard(k);return best
def uci(m):
 r=""
 for x in(m&127,m>>7&127):r+=chr(97+(x&7))+chr(56-(x>>4))
 if m>>14&7:r+="nbrq"[(m>>14&7)-2]
 return r
U=os.environ["MAKEFUN_ENV_URL"]
while 1:
 g=json.load(R.urlopen(U+"/game",data=b"{}"))
 if g["status"]=="done":break
 HS=set()
 while g["status"]=="your_move":
  p=parse(g["fen"]);N=ST=0;bm=(gen(p)or[0])[0];HS.discard((tuple(p[0]),*p[1:]))
  for d in range(64):
   sr(p,-200000,200000,d);e=TT.get((tuple(p[0]),*p[1:]),0)
   if e:bm=e
   if ST or N>1e5:break
  if not bm:break
  q=mk(p,bm);HS.add((tuple(p[0]),*p[1:]));HS.add((tuple(q[0]),*q[1:]))
  g=json.load(R.urlopen(U+"/move",data=json.dumps({"game":g["game"],"move":uci(bm)}).encode()))
