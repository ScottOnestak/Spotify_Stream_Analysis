#title: Song Analysis
#author: Scott Onestak
#updated: May 2023

library(dplyr)
library(jsonlite)
library(curl)
library(glue)
library(FactoMineR)
library(ggplot2)
library(tidyr)
library(zoo)
library(stringr)

#Read in song dataset
songs_raw = read.csv('sonestak.csv',header=F,stringsAsFactors = F)
colnames(songs_raw) = c('Artist','Album','Track','Timestamp')

#Cleaning
songs = songs_raw %>% filter(Timestamp != '')

#Distinct Tracks
distinct_tracks = as.data.frame(unique(songs$Track))
colnames(distinct_tracks) = c("Track")
distinct_tracks = distinct_tracks %>% arrange(Track)

#Grouping Same Songs Together
songs$Track[songs$Track =="starstrukk (feat. katyperry)"] = "Starstrukk (feat. Katy Perry)"
songs$Track[songs$Track == "The Chipmunk Song (Christmas Don't Be Late) - Remastered" | 
              songs$Track == "The Chipmunk Song (Christmas Don't Be Late) - Remastered 1999" ] = "The Chipmunk Song (Christmas Don't Be Late)"
songs$Track[songs$Track == "Be Our Guest - From Beauty and the Beast/Soundtrack" | 
              songs$Track == "Beauty and the Beast - From Beauty and the Beast/Soundtrack Version" ] = "Be Our Guest - From Beauty and the Beast"
songs$Track[songs$Track == "Cups (Pitch PerfectÃ¢???Ts Ã¢???oWhen IÃ¢???Tm GoneÃ¢???Â) - Pop Version" |
              songs$Track == "Cups (Pitch PerfectÃ¢???Ts Ã¢???oWhen IÃ¢???Tm GoneÃ¢???Â)"] = "Cups (Pitch Perfect's 'When I'm Gone')"
songs$Track[songs$Track == "YouÃ¢???Tll Never Know"] = "You'll Never Know"
songs$Track[songs$Track == "I Could Be The One (Avicii Vs. Nicky Romero) - Nicktim / Radio Edit" |
              songs$Track == "I Could Be The One [Avicii vs Nicky Romero] - Nicktim - Radio Edit" |
              songs$Track == "I Could Be The One [Avicii vs Nicky Romero] - Nicktim / Radio Edit"] =
  "I Could Be The One (Avicii Vs. Nicky Romero) - Radio Edit"
songs$Artist[songs$Artist == "Axwell Ã> Ingrosso" |
               songs$Artist == "Axwell / Ingrosso"] = "Axwell /\ Ingrosso"
songs$Track[songs$Track == "The Way I Are (Dance with Somebody) [feat. Lil Wayne]"] = "The Way I Are (Dance with Somebody)"
songs$Track[songs$Track == "Heaven Is A Place On Earth - 2009 Digital Remaster"] = "Heaven Is A Place on Earth"
songs$Artist[songs$Artist == "BeyoncÃÂ©"] = "Beyonce"
songs$Track[songs$Track == "Mony Mony - Downtown Mix / 24-Bit Digitally Remastered 2001"] = "Mony Mony - 24-Bit Digitally Remastered 01"
songs$Track[songs$Track == "Call Me - Digitally Remastered 98"] = "Call Me"
songs$Artist[songs$Artist == "Bobby 'Boris' Pickett"] = "Bobby Boris Pickett & The Crypt-Kickers"
songs$Track[songs$Track == "Rockin' Around The Christmas Tree - Single Version"] = "Rockin' Around The Christmas Tree"
songs$Track[songs$Track == "All This Time - Pop Mix"] = "All This Time"
songs$Track[songs$Track == "Be The Change - Radio Version"] = "Be the Change"
songs$Track[songs$Track == "Gold (Jason Nevins Rhythmic Radio) Remix"] = "Gold - Jason Nevins Rhythmic Remix/Bonus Track"
songs$Track[songs$Track == "Ready or Not - feat. Lecrae"] = "Ready or Not"
songs$Track[songs$Track == "Santa Claus Is Comin' to Town - C.W. Post College Greenvale NY - December 1975"] = "Santa Claus Is Comin' To Town - Single Version"
songs$Track[songs$Track == "A Holly Jolly Christmas - Single Version"] = "A Holly Jolly Christmas"
songs$Artist[songs$Artist == "CÃÂ©line Dion"] = "Celine Dion"
songs$Track[songs$Track == "Ashes - from the Deadpool 2 Motion Picture Soundtrack" |
              songs$Track == "Ashes - from Deadpool 2 Motion Picture Soundtrack"] = "Ashes - from Deadpool 2"
songs$Track[songs$Track == "Because You Loved Me (Theme from Up Close and Personal)"] = "Because You Loved Me"
songs$Track[songs$Track == "My Heart Will Go On - Love Theme From Titanic"] = "My Heart Will Go On"
songs$Track[songs$Track == "I Need Your Love (feat. Ellie Goulding)"] = "I Need Your Love"
songs$Track[songs$Track == "Outside (feat. Ellie Goulding)"] = "Outside"
songs$Track[songs$Track == "EÃ¢???Â¢MOÃ¢???Â¢TION"] = "Emotion"
songs$Track[songs$Track == "I DidnÃ¢???Tt Just Come Here To Dance"] = "I Didn't Just Come Here To Dance"
songs$Track[songs$Track == "LetÃ¢???Ts Get Lost"] = "Let's Get Lost"
songs$Track[songs$Track == "Tonight IÃ¢???Tm Getting Over You"] = "Tonight I'm Getting Over You"
songs$Track[songs$Track == "Tonight IÃ¢???Tm Getting Over You - Reid Stefan Remix"] = "Tonight I'm Getting Over You - Reid Stefan Remix"
songs$Track[songs$Track == "Tonight IÃ¢???Tm Getting Over You - Remix"] = "Tonight I'm Getting Over You - Remix"
songs$Artist[songs$Artist == "Carly Rae Jepsen | www.soundfox.co"] = "Carly Rae Jepsen"
songs$Track[songs$Track == "Circle Of Life - From The Lion King/Soundtrack" |
              songs$Track == "Circle of Life"] = "Circle of Life - From The Lion King"
songs$Track[songs$Track == "Ever Ever After - From Enchanted / Soundtrack Version"] = "Ever Ever After - Soundtrack"
songs$Track[songs$Track == "Make Your Own Kind Of Music - Remastered - Single Version"] = "Make Your Own Kind of Music"
songs$Track[songs$Track == "Sex (Cheat Codes X Kris Kross Amsterdam)"] = "Sex"
songs$Track[songs$Track == "Reflection - From Mulan / Pop Version" |
              songs$Track == "Reflection - Pop Version"] = "Reflection"
songs$Track[songs$Track == "We Remain - From Ã¢???oThe Hunger Games: Catching FireÃ¢???Â/ Soundtrack"] = "We Remain - From 'The Hunger Games: Catching Fire' Soundtrack"
songs$Track[songs$Track == "CanÃ¢???Tt Help Falling In Love - The Voice Performance"] = "Can't Help Falling In Love - The Voice Performance"
songs$Track[songs$Track == "Hold On WeÃ¢???Tre Going Home - The Voice Performance"] = "Hold On We're Going Home - The Voice Performance"
songs$Track[songs$Track == "I WonÃ¢???Tt Give Up - The Voice Performance"] = "I Won't Give Up"
songs$Track[songs$Track == "When You Wish Upon a Star - From Pinocchio"] = "When You Wish Upon a Star"
songs$Track[songs$Track == "Dream Ghost (feat. Michael Hyatt Amber Riley & Ricki Lake)"] = "Dream Ghost"
songs$Track[songs$Track == "GregÃ¢???Ts Drinking Song"] = "Greg's Drinking Song"
songs$Track[songs$Track == "IÃ¢???Tm A Good Person" |
              songs$Track == "I'm a Good Person (feat. Rachel Bloom)"] = "I'm A Good Person"
songs$Artist[songs$Artist == "Ã'ÃÂµÃ???ÃÂºÃÂ° ÃÂ¡ÃÂµÃ???ÃÂ´ÃZÃ???ÃÂºÃÂ°"] = "Verka Serduchka"
songs$Track[songs$Track == "Hey Mama (feat. Nicki Minaj & Afrojack)"] = "Hey Mama (feat. Nicki Minaj Bebe Rexha & Afrojack)"
songs$Track[songs$Track == "Titanium"] = "Titanium (feat. Sia)"
songs$Track[songs$Track == "When Love Takes Over (feat. Kelly Rowland) - feat. Kelly Rowland"] = "When Love Takes Over (feat. Kelly Rowland)"
songs$Track[songs$Track == "Where Them Girls At - feat. Nicki Minaj & Flo Rida"] = "Where Them Girls At (feat. Nicki Minaj & Flo Rida)"
songs$Track[songs$Track == "Pour Some Sugar On Me - Remastered 2017"] = "You Spin Me Round (Like a Record)"
songs$Track[songs$Track == "Listen To Your Heart (EdmÃÂ©e's Unplugged Vocal)"] = "Listen To Your Heart (Edmee's Unplugged Vocal)"
songs$Track[songs$Track == "Cake By The Ocean - Ã£,Â¯Ã£fÂªÃ£fÂ¼Ã£fÂ³Ã£fÂ»Ã£fÂ´Ã£,Â¡Ã£fÂ¼Ã£,Â¸Ã£fÂ§Ã£fÂ³"] = "Cake by the Ocean"
songs$Track[songs$Track == "Stereo Love Featuring Vika Jigulina - Radio Edit"] = "Stereo Love - Radio Edit"
songs$Track[songs$Track == "Hanging On - Edit"] = "Hanging On"
songs$Track[songs$Track == "Still Falling For You - From Bridget Jones's Baby Original Motion Picture Soundtrack"] = "Still Falling For You - From Bridget Jones's Baby"
songs$Track[songs$Track == "Can You Feel The Love Tonight - End Title / From The Lion King"] = "Can You Feel the Love Tonight"
songs$Track[songs$Track == "When IÃ¢???Tm Alone"] = "When I'm Alone"
songs$Track[songs$Track == "There You'll Be - Remastered Version"] = "There You'll Be - Remastered"
songs$Track[songs$Track == "Work from Home (feat. Ty Dolla $ign)" |
              songs$Track == "Work from Home"] = "Work From Home"
songs$Track[songs$Track == "100 Years: Music Video"] = "100 Years"
songs$Track[songs$Track == "Right Round - US"] = "Right Round"
songs$Track[songs$Track == "Say It"] = "Say It (feat. Tove Lo)"
songs$Track[songs$Track == "We Are Young (feat. Janelle MonÃÂ¡e) - feat. Janelle Monae" |
              songs$Track == "We Are Young (feat. Janelle MonÃÂ¡e)" |
              songs$Track == "We Are Young (feat. Janelle MonÃ¡e)" |
              songs$Track == "We Are Young (feat. Janelle MonÃ¡e) - feat. Janelle Monae"] = "We Are Young (feat. Janelle Monae)"
songs$Track[songs$Track == "Saving Light"] = "Saving Light (feat. Haliene)"
songs$Track[songs$Track == "Feel Good (feat. Daya)"] = "Feel Good"
songs$Track[songs$Track == "I Got Nerve - From Hannah Montana/Soundtrack Version"] = "I Got Nerve"
songs$Track[songs$Track == "Nobody's Perfect - From Hannah Montana 2"] = "Nobody's Perfect"
songs$Track[songs$Track == "The Best of Both Worlds - From Hannah Montana/Soundtrack Version"] = "The Best of Both Worlds"
songs$Track[songs$Track == "The Other Side of Me - From Hannah Montana/Soundtrack Version"] = "The Other Side of Me"
songs$Track[songs$Track == "Christ Is Enough - Live"] = "Christ Is Enough"
songs$Artist[songs$Artist == "Hixxy"] = "Gareth Emery"
songs$Track[songs$Track == "Saving Light (Hixxy Remix) [feat. HALIENE]"] = "Saving Light - Hixxy Remix"
songs$Track[songs$Track == "Black Widow (feat. Rita Ora) - Clean Edit"] = "Black Widow"
songs$Artist[songs$Artist == "Jason DerÃÂ¼lo"] = "Jason Derulo"
songs$Track[songs$Track == "Flashlight - From Pitch Perfect 2 Soundtrack"] = "Flashlight - From Pitch Perfect 2"
songs$Track[songs$Track == "Part Of Your World - From The Little Mermaid/ Soundtrack Version"] = "Part of Your World - From The Little Mermaid / Soundtrack Version"
songs$Track[songs$Track == "Happy Xmas (War Is Over) - Remastered" |
              songs$Track == "Happy Xmas (War Is Over) - Remastered 2010"] = "Happy Xmas (War Is Over) - 2010 Digital Remaster"
songs$Artist[songs$Artist == "John Martyn"] = "John Martin"
songs$Track[songs$Track == "St. Elmos Fire [Man In Motion]" |
              songs$Track == "St. Elmos Fire (Man in Motion)"] = "St. Elmo's Fire (Man in Motion)"
songs$Track[songs$Track == "It's Beginning To Look Like Christmas"] = "It's Beginning to Look a Lot Like Christmas"
songs$Track[songs$Track == "Too Little Too Late - Radio Version"] = "Too Little Too Late"
songs$Artist[songs$Artist == "JosÃÂ© Feliciano" |
               songs$Artist == "JosÃ© Feliciano"] = "Jose Feliciano"
songs$Track[songs$Track == "Can You Feel The Love Tonight - From The Lion King/Soundtrack Version"] = "Can You Feel the Love Tonight"
songs$Track[songs$Track == "Angels We Have Heard On High (with Brian McKnight)"] = "Angels We Have Heard On High - Duet With Brian Mcknight"
songs$Track[songs$Track == "O Come All Ye Faithful (With The Mormon Tabernacle Choir under the direction of Craig Jessop) - with The Mormon Tabernacle Choir under the direction of Craig Jessop Album Version" | 
              songs$Track == "O Come All Ye Faithful - with The Mormon Tabernacle Choir under the direction of Craig Jessop"] = "O Come All Ye Faithful (with The Mormon Tabernacle Choir under the direction of Craig Jessop)"
songs$Track[songs$Track == "The First NoÃÂ«l - duet with Faith Hill" | 
              songs$Track == "The First NoÃÂ«l (with Faith Hill) - duet with Faith Hill Album Version"] = "The First Noel - Duet With Faith Hill"
songs$Track[songs$Track == "California Gurls - feat. Snoop Dogg"] = "California Gurls (feat. Snoop Dogg)"
songs$Track[songs$Track == "Dark Horse (feat. Juicy J)"] = "Dark Horse"
songs$Track[songs$Track == "E.T. - feat. Kanye West"] = "	E.T."
songs$Track[songs$Track == "Heartbeat Song - Dave AudÃÂ© Radio Mix" |
              songs$Track == "Heartbeat Song - Dave AudÃ© Radio Mix"] = "Heartbeat Song - Dave Aude Radio Mix"
songs$Track[songs$Track == "Carry Me (feat. Julia Michaels)"] = "Carry Me"
songs$Track[songs$Track == "It AinÃ¢???Tt Me (with Selena Gomez)"] = "It Ain't Me"
songs$Track[songs$Track == "ScheiÃYe" | songs$Track == "ScheiÃŸe"] = "Scheibe"
songs$Track[songs$Track == "Shallow - Radio Edit"] = "Shallow"
songs$Track[songs$Track == "Telephone (feat. BeyoncÃÂ©)"] = "Telephone (feat. Beyonce)"
songs$Track[songs$Track == "YoÃÂ¼ and I"] = "You and I"
songs$Track[songs$Track == "Summertime Sadness (Lana Del Rey Vs. Cedric Gervais) - Cedric Gervais Remix" |
              songs$Track == "Summertime Sadness [Lana Del Rey vs. Cedric Gervais] - Cedric Gervais Remix"] = "Summertime Sadness (Lana Del Rey Vs. Cedric Gervais) - Cedric Gervais Remix / Radio Edit"
songs$Track[songs$Track == "A Whole New World - From Aladdin/ Soundtrack Version"] = "A Whole New World"
songs$Track[songs$Track == "God Bless The U.S.A. - Re-Recorded In Stereo" |
              songs$Track == "God Bless The U.S.A."] = "God Bless the USA"
songs$Track[songs$Track == "Longer Than I Thought (feat. Joe Jonas)"] = "Longer Than I Thought"
songs$Track[songs$Track == "Too Far Gone (feat. Anna Clendening)"] = "Too Far Gone"
songs$Track[songs$Track == "Dominick the Donkey (The Italian Christmas Donkey)"] = "Dominick The Italian Christmas Donkey"
songs$Track[songs$Track == "What A Wonderful World - Single Version"] = "What a Wonderful World"
songs$Track[songs$Track == "Bad Things - With Camila Cabello"] = "Bad Things"
songs$Track[songs$Track == "I Won't Let You Walk Away (feat. Madison Beer) - Lost Kings Remix"] = "I Won't Let You Walk Away - Lost Kings Remix"
songs$Track[songs$Track == "I Won't Let You Walk Away (feat. Madison Beer) - Radio Edit" |
              songs$Track == "I Won't Let You Walk Away (feat. Madison Beer) - Pop Radio Edit"] = "I Won't Let You Walk Away - Radio Edit"
songs$Artist[songs$Artist == "Marina & the Diamonds"] = "Marina"
songs$Track[songs$Track == "She Will Be Loved - Radio Mix"] = "She Will Be Loved"
songs$Artist[songs$Artist == "MGM Studio Orchestra"] = "MGM Studio Chorus"
songs$Artist[songs$Artist == "Molly SandÃÂ©n" | songs$Artist = "Molly SandÃ©n"] = "Molly Sanden"
songs$Artist[str_detect(songs$Artist,"Molly Sand")] = "Molly Sanden"
songs$Artist[str_detect(songs$Artist,"GATT")] = "GATTUSO"
songs$Track[songs$Track == "O Tannenbaum - Remastered 1999"] = "O Tannenbaum"
songs$Track[songs$Track == "Hakuna Matata - From The Lion King/Soundtrack"] = "Hakuna Matata"
songs$Artist[songs$Artist == "Owl City & Carly Rae Jepsen"] = "Owl City"
songs$Track[songs$Track == "YouÃ¢???Tre Not Alone"] = "You're Not Alone"
songs$Track[songs$Track == "Can We Pretend (feat. Cash Cash)"] = "Can We Pretend"
songs$Track[songs$Track == "ThereÃ¢???Ts A Honey"] = "There's A Honey"
songs$Track[songs$Track == "Hit Me With Your Best Shot - 1999 Digital Remaster"] = "Hit Me With Your Best Shot"
songs$Track[songs$Track == "You'll Be In My Heart (Phil Version) - Ã£???ZÃ£,Â¿Ã£fÂ¼Ã£,Â¶Ã£fÂ³Ã£???ÂÃ£,^Ã£,S" |
              songs$Track == "You'll Be In My Heart - From Tarzan/Soundtrack Version" |
              songs$Track == "You Will Be In My Heart"] = "You'll Be In My Heart"
songs$Track[songs$Track == "QuÃÂ© Sera Sera" | songs$Track == "QuÃ© Sera Sera"] = "Que Sera Sera"
songs$Artist[songs$Artist == "Raven-SymonÃÂ©" | songs$Artist == "Raven-SymonÃ©"] = "Raven-Symone"
songs$Track[songs$Track == "We Found Love (Album Version) [Feat. Calvin Harris]"] = "We Found Love (feat. Calvin Harris)"
songs$Track[songs$Track == "Go the Distance - Ã£???ZÃ£f~Ã£fÂ©Ã£,Â¯Ã£fÂ¬Ã£,Â¹Ã£???ÂÃ£,^Ã£,S" |
              songs$Track == "Go the Distance - From Hercules / Soundtrack Version" |
              songs$Track == "Go The Distance - From Hercules/Soundtrack"] = "Go The Distance"
songs$Track[songs$Track == "Under the Sea - From The Little Mermaid / Soundtrack Version" |
              songs$Track == "Under the Sea - From The Little Mermaid"] = "Under The Sea - From The Little Mermaid/ Soundtrack Version"
songs$Track[songs$Track == "When She Loved Me - From Toy Story 2/Soundtrack Version" |
              songs$Track == "When She Loved Me - From Toy Story 2"] = "When She Loved Me"
songs$Track[songs$Track == 'Back To You - From 13 Reasons Why Ã¢???" Season 2 Soundtrack' |
              songs$Track == 'Back to You - From 13 Reasons Why â€“ Season 2 Soundtrack'] = "Back To You"
songs$Track[songs$Track == "DonÃ¢???Tt Leave" | songs$Track == "Donâ€™t Leave"] = "Don't Leave"
songs$Track[songs$Track == "First Time (feat. Dylan Matthew)"] = "First Time"
songs$Track[songs$Track == "Easy Love - Original Mix"] = "Easy Love"
songs$Track[songs$Track == "Sweet Lovin' - Original Mix" |
              songs$Track == "Sweet Lovin' - Radio Edit"] = "Sweet Lovin'"
songs$Track[songs$Track == "All My Friends (feat. Tinashe & Chance The Rapper)"] = "All My Friends"
songs$Track[songs$Track == "Live Love Learn (feat. Peg Parnevik)"] = "Live Love Learn"
songs$Track[songs$Track == "Wannabe - Radio Edit"] = "Wannabe"
songs$Track[songs$Track == "I Won't Say (I'm In Love) - Ã£???ZÃ£f~Ã£fÂ©Ã£,Â¯Ã£fÂ¬Ã£,Â¹Ã£???ÂÃ£,^Ã£,S" |
              songs$Track == "I Won't Say (I'm In Love) - From Hercules"] = "I Won't Say (I'm In Love)"
songs$Track[songs$Track == "Don't You Worry Child (Radio Edit) [feat. John Martin]" |
              songs$Track == "Don't You Worry Child (Radio Edit)"] = "Don't You Worry Child - Radio Edit"
songs$Track[songs$Track == "...Ready for It?"] = "... Ready For It?"
songs$Track[songs$Track == "Bellas Finals: Price Tag/DonÃ¢???Tt You (Forget About Me)/Give Me Everything/Just The Way You Are/Party In The U.S.A./Turn The Beat Around"] = "Bellas Finals: Price Tag/Don't You (Forget About Me)/Give Me Everything/Just The Way You Are/Party In The U.S.A./Turn The Beat Around"
songs$Track[songs$Track == "Cups (Ã¢???oWhen IÃ¢???Tm GoneÃ¢???Â) - Campfire Version"] = "Cups ('When I'm Gone') - Campfire Version"
songs$Track[songs$Track == "Cups (Pitch PerfectÃ¢â¬â¢s Ã¢â¬ÅWhen IÃ¢â¬â¢m GoneÃ¢â¬Â)"] = "Cups ('When I'm Gone')"
songs$Track[songs$Track == "Cups (Pitch PerfectÃ¢â¬â¢s Ã¢â¬ÅWhen IÃ¢â¬â¢m GoneÃ¢â¬Â) - Pop Version"] = "Cups ('When I'm Gone')"
songs$Track[songs$Track == "Riff Off: Mickey/Like A Virgin/Hit Me With Your Best Shot/S&M/LetÃ¢???Ts Talk About Sex/IÃ¢???Tll Make Love To You/Feels Like The First Time/No Diggity"] = "Riff Off: Mickey/Like A Virgin/Hit Me With Your Best Shot/S&M/Let's Talk About Sex/I'll Make Love To You/Feels Like The First Time/No Diggity"
songs$Track[songs$Track == "Do You Mean (with Ty Dolla $ign & bÃÂ¼low)" |
              songs$Track == "Do You Mean (with Ty Dolla $ign & bÃ¼low)"] = "Do You Mean"
songs$Track[songs$Track == "Side Effects (feat. Emily Warren)"] = "Side Effects"
songs$Track[songs$Track == "Takeaway"] = "Takeaway (with ILLENIUM & Lennon Stella)"
songs$Track[songs$Track == "Our Lips Are Sealed - Single Version"] = "Our Lips Are Sealed"
songs$Track[songs$Track == "We Got The Beat - Single Mix"] = "We Got the Beat"
songs$Artist[songs$Artist == "TiÃÂ«sto" | songs$Artist == "TiÃ«sto"] = "Tiesto"
songs$Track[songs$Track == "IÃ¢???Tm Me" | songs$Track == "Iâ€™m Me"] = "I'm Me"
songs$Track[songs$Track == "Another ShotÃ¢???Â¬Ã¢???Â¬Ã¢???Â¬"] = "Another Shot"
songs$Track[songs$Track == "WeÃ¢???Tre The Same"] = "We're the Same"
songs$Track[songs$Track == "	I Have Nothing - Remastered"] = "I Have Nothing"
songs$Track[songs$Track == "The Star Spangled Banner (feat. The Florida Orchestra) - Live from Super Bowl XXV"] = "The Star Spangled Banner - Live from Super Bowl XXV"
songs$Track[songs$Track == "Clarity - feat. Foxes"] = "Clarity"
songs$Track[songs$Track == "Happy Now (with Elley DuhÃÂ©)"] = "Happy Now"
songs$Track[songs$Track == "I Want You to Know (feat. Selena Gomez)"] = "I Want You to Know"
songs$Artist[songs$Artist == "D.H.T."] = "DHT"
songs$Track[songs$Track == "Listen to Your Heart (featuring Edmee) - Furious F. EZ Radio Edit"] = "Listen To Your Heart (Furious F. EZ Radio Edit)"
songs$Track[songs$Track == "You Are Loved (Don't Give Up) - Don't Give Up Album Version"] = "You Are Loved (Don't Give Up)"
songs$Artist[songs$Artist == "Joshua Coleman"] = "Fifth Harmony"
songs$Track[songs$Track == " I Won't Say (I'm In Love)"] = "I Won't Say (I'm In Love)"
songs$Track[songs$Track == "Friday NightÃ¢???Â¬Ã¢???Â¬Ã¢???Â¬" | songs$Track == "Friday Nightâ€¬â€¬â€¬"] = "Friday Night"
songs$Track[songs$Track == "I Have Nothing - Remastered"] = "I Have Nothing"
songs$Track[songs$Track == "Ashes - from Deadpool 2"] = "Ashes"
songs$Artist[songs$Artist == "Emeli SandÃÂ©"] = "Emeli Sande"
songs$Artist[songs$Artist == "Emeli SandÃ©"] = "Emeli Sande"
songs$Track[songs$Track == "Anywhere For You"] = "Anywhere For You - Radio Edit"
songs$Track[songs$Track == "You Keep Me Hangin' On"] = "You Keep Me Hangin On"
songs$Artist[songs$Artist == "Bethel Music"] = "Kristene DiMarco"
songs$Artist[songs$Artist == "Mama Cass"] = "Cass Elliot"
songs$Track[songs$Track == "Jump - 2015 Remastered Version"] = "Jump"
songs$Track[songs$Track == "doN'T StArT nOw"] = "Don't Start Now"
songs$Artist[songs$Artist == "RedOne"] = "Red One"
songs$Track[songs$Track == "Maybe YouÃ¢â¬â¢re the Problem"] = "Maybe You're the Problem"
songs$Artist[songs$Artist == "Marina and the Diamonds"] = "Marina"
songs$Track[songs$Track == "NÃÂ¥n annan nu"] = "Nan annan nu"
songs$Track[songs$Track == "Vi ska aldrig gÃÂ¥ hem"] = "Vi ska aldrig ga hem"
songs$Track[songs$Track == "StÃÂ¶rre"] = "Storre"
songs$Track[songs$Track == "When IÃ¢â¬â¢m Gone (with Katy Perry)"] = "When I'm Gone (with Katy Perry)"
songs$Track[songs$Track == "YouÃ¢â¬â¢ll Never Know"] = "You'll Never Know"
songs$Track[songs$Track == "One Way Or Another - Live"] = "One Way or Another"
songs$Track[songs$Track == "Work B**ch"] = "Work Bitch"
songs$Track[songs$Track == "This Is What You Came For (feat. Rihanna)"] = "This Is What You Came For"
songs$Track[songs$Track == "I DidnÃ¢â¬â¢t Just Come Here To Dance"] = "I Didn't Just Come Here To Dance"
songs$Track[songs$Track == "LetÃ¢â¬â¢s Get Lost"] = "Let's Get Lost"
songs$Track[songs$Track == "Tonight IÃ¢â¬â¢m Getting Over You"] = "Tonight I'm Getting Over You"
songs$Track[songs$Track == "Tonight IÃ¢â¬â¢m Getting Over You - Remix"] = "Tonight I'm Getting Over You - Remix"
songs$Artist[songs$Artist == "Carolina GaitÃÂ¡n - La Gaita"] = "Carolina Gaitan - La Gaita"
songs$Track[songs$Track == "CanÃ¢â¬â¢t Help Falling In Love - The Voice Performance"] = "Can't Help Falling In Love - The Voice Performance"
songs$Track[songs$Track == "Hold On WeÃ¢â¬â¢re Going Home - The Voice Performance"] = "Hold On We're Going Home - The Voice Performance"
songs$Track[songs$Track == "I WonÃ¢â¬â¢t Give Up - The Voice Performance"] = "I Won't Give Up - The Voice Performance"
songs$Artist[songs$Artist == "ÃâÃÂµÃâ¬ÃÂºÃÂ° ÃÂ¡ÃÂµÃâ¬ÃÂ´ÃÅ½Ãâ¡ÃÂºÃÂ°"] = "Verka Serduchka"
songs$Track[songs$Track == "Listen To Your Heart - Furious F. EZ Radio edit"] = "Listen To Your Heart (Furious F. EZ Radio Edit)"
songs$Track[songs$Track == "Heaven (featuring Do)"] = "Heaven"
songs$Track[songs$Track == "Cake By The Ocean - Ã£âÂ¯Ã£ÆÂªÃ£ÆÂ¼Ã£ÆÂ³Ã£ÆÂ»Ã£ÆÂ´Ã£âÂ¡Ã£ÆÂ¼Ã£âÂ¸Ã£ÆÂ§Ã£ÆÂ³"] = "Cake by the Ocean"
songs$Track[songs$Track == "If It AinÃ¢â¬â¢t Me"] = "If It Ain't Me"
songs$Track[songs$Track == "Wildflowers - From Ã¢â¬ÅTeen SpiritÃ¢â¬Â Soundtrack"] = "Wildflowers"
songs$Track[songs$Track == "Lights - Single Version"] = "Lights"
songs$Track[songs$Track == "Call On Me - Radio Mix"] = "Call On Me - Radio Edit"
songs$Track[songs$Track == "California Gurls (feat. Snoop Dogg)"] = "California Gurls"
songs$Track[songs$Track == "Santa CanÃ¢â¬â¢t You Hear Me"] = "Santa Can't You Hear Me"
songs$Track[songs$Track == "i donÃ¢â¬â¢t wanna dieÃ¢â¬Â¦"] = "i don't wanna die"
songs$Track[songs$Track == "Tell Me ItÃ¢â¬â¢s A Nightmare"] = "Tell Me It's a Nightmare"
songs$Track[songs$Track == "Turn Off the Light (feat. Elvira Mistress of the Dark)"] = "Turn Off The Light"
songs$Track[songs$Track == "You Keep Me Hanging On"] = "You Keep Me Hangin On"
songs$Track[songs$Track == "It AinÃ¢â¬â¢t Me (with Selena Gomez)"] = "It Ain't Me (with Selena Gomez)"
songs$Track[songs$Track == "ScheiÃÅ¸e"] = "Scheibe"
songs$Track[songs$Track == "Summertime Sadness (Lana Del Rey vs. Cedric Gervais) - Cedric Gervais Remix"] = "Summertime Sadness (Lana Del Rey Vs. Cedric Gervais) - Cedric Gervais Remix / Radio Edit"
songs$Track[songs$Track == "I WonÃ¢â¬â¢t Let You Walk Away - Radio Edit" |
              songs$Track == "I Wonâ€™t Let You Walk Away - Radio Edit"] = "I Won't Let You Walk Away - Radio Edit"
songs$Track[songs$Track == "Dag fÃÂ¶r dag" | songs$Track == "Dag fÃ¶r dag"] = "Dag for dag"
songs$Track[songs$Track == "Det bÃÂ¤sta kanske inte hÃÂ¤nt ÃÂ¤n" |
              songs$Track == "Det bÃ¤sta kanske inte hÃ¤nt Ã¤n"] = "Det basta kanske inte hant an"
songs$Track[songs$Track == "NÃ¥n annan nu"] = "Nan annan nu"
songs$Track[songs$Track == "StÃ¶rre"] = "Storre"
songs$Track[songs$Track == "Va det dÃÂ¥?" | songs$Track == "Va det dÃ¥?"] = "Va det da?"
songs$Track[songs$Track == "Vi ska aldrig gÃ¥ hem"] = "Vi ska aldrig ga hem"
songs$Track[songs$Track == "Vi ska aldrig gÃÂ¥ hem - Nause & Allertz Remix" |
              songs$Track == "Vi ska aldrig gÃ¥ hem - Nause & Allertz Remix"] = "Vi ska aldrig ga hem - Nause & Allertz Remix"
songs$Track[songs$Track == "YouÃ¢â¬â¢re Not Alone" | songs$Track == "Youâ€™re Not Alone"] = "You're Not Alone"
songs$Track[songs$Track == "Feel This Moment (feat. Christina Aguilera)"] = "Feel This Moment"
songs$Track[songs$Track == "Timber (feat. Ke$ha)"] = "Timber"
songs$Track[songs$Track == "R.I.P. (feat. Tinie Tempah)"] = "R.I.P."
songs$Track[songs$Track == "Back to You - From 13 Reasons Why Ã¢â¬â Season 2 Soundtrack"] = "Back to You"
songs$Track[songs$Track == "DonÃ¢â¬â¢t Leave"] = "Don't Leave"
songs$Track[songs$Track == "Hips Don't Lie (feat. Wyclef Jean)"] = "Hips Don't Lie"
songs$Track[songs$Track == "The Greatest (feat. Kendrick Lamar)"] = "The Greatest"
songs$Track[songs$Track == "All My Friends (feat. Tinashe & Chance the Rapper)"] = "All My Friends"
songs$Track[songs$Track == "If YouÃ¢â¬â¢re Too Shy (Let Me Know) - Edit" |
              songs$Track == "If Youâ€™re Too Shy (Let Me Know) - Edit"] = "If You're Too Shy (Let Me Know) - Edit"
songs$Track[songs$Track == "Bellas Finals: Price Tag/DonÃ¢â¬â¢t You (Forget About Me)/Give Me Everything/Just The Way You Are/Party In The U.S.A./Turn The Beat Around" |
              songs$Track == "Bellas Finals: Price Tag/Donâ€™t You (Forget About Me)/Give Me Everything/Just The Way You Are/Party In The U.S.A./Turn The Beat Around"] = "Bellas Finals: Price Tag/Don't You (Forget About Me)/Give Me Everything/Just The Way You Are/Party In The U.S.A./Turn The Beat Around"
songs$Track[songs$Track == "Cups (Ã¢â¬ÅWhen IÃ¢â¬â¢m GoneÃ¢â¬Â) - Campfire Version" | 
              songs$Track == "Cups (â€œWhen Iâ€™m Goneâ€) - Campfire Version"] = "Cups (When I'm Gone) - Campfire Version"
songs$Track[songs$Track == "Don't Let Me Down (feat. Daya)"] = "Don't Let Me Down"
songs$Track[songs$Track == "Don't Let Me Down (feat. Daya) - Illenium Remix"] = "Don't Let Me Down - Illenium Remix"
songs$Track[songs$Track == "Don't Let Me Down (feat. Daya) - Hardwell & Sephyx Remix"] = "Don't Let Me Down - Hardwell & Sephyx Remix"
songs$Track[songs$Track == "Takeaway (feat. Lennon Stella) - Pilton Remix"] = "Takeaway - Pilton Remix"
songs$Track[songs$Track == "Take My Breath - single version"] = "Take My Breath"
songs$Track[songs$Track == "Friday NightÃ¢â¬Â¬Ã¢â¬Â¬Ã¢â¬Â¬"] = "Friday Night"
songs$Track[songs$Track == "WeÃ¢â¬â¢re The Same" | songs$Track == "Weâ€™re The Same"] = "We're The Same"
songs$Artist[songs$Artist == "GATTÃÅSO"] = "GATTUSO"
songs$Track[songs$Track == "Santa Claus Is Comin' to Town - Live at C.W. Post College Greenvale NY - December 1975"] = "Santa Claus Is Comin' To Town - Single Version"
songs$Track[songs$Track == "<demons>"] = "demons"
songs$Track[songs$Track == "About Love - From The Netflix Film Ã¢â¬ÅTo All The Boys: P.S. I Still Love YouÃ¢â¬\u009d" |
              songs$Track == "About Love - From The Netflix Film â€œTo All The Boys: P.S. I Still Love Youâ€	" |
              songs$Track == "About Love - From The Netflix Film â€œTo All The Boys: P.S. I Still Love Youâ€"] = "About Love"
songs$Track[songs$Track == "The Stand Off (I Want You!) [feat. Hayley May]"] = "The Stand Off (I Want You!)"
songs$Track[songs$Track == "IÃ¢â¬â¢m Me"] = "I'm Me"
songs$Track[songs$Track == "Right Now - GATTÃÅSO Remix" |
              songs$Track == "Right Now - GATTÃœSO Remix"] = "Right Now - GATTUSO Remix"
songs$Track[songs$Track == "GATTÃœSO"] = "GATTUSO"
songs$Track[songs$Track == "Rain On Me - Purple Disco Machine Remix - Edit"] = "Rain On Me - Purple Disco Machine Remix"
songs$Artist[songs$Track == "You Spin Me Round (Like a Record)" & songs$Artist=="Def Leppard"] = "Dead or Alive"
songs$Track[songs$Track == "Super Natural (feat. Carly Rae Jepsen)"] = "Super Natural"
songs$Artist[songs$Artist == "Black Eyed Peas"] = "The Black Eyed Peas"
songs$Artist[songs$Artist == "Harley Bird"] = "Jim Yosef"
songs$Track[songs$Track == "Uptown Funk (feat. Bruno Mars)"] = "Uptown Funk"
songs$Track[songs$Track == "Christmas Eve / Sarajevo 12/24 - Instrumental"] = "Christmas Eve/ Sarajevo"
songs$Track[songs$Track == "Wildstar (feat. Foxes)"] = "Wildstar"
songs$Track[songs$Track == "Troublemaker (feat. Flo Rida)"] = "Troublemaker"
songs$Artist[songs$Track == "You're a Mean One Mr. Grinch" & songs$Artist=="Burl Ives"] = "Thurl Ravenscroft"
songs$Track[songs$Track == "You're a Mean One Mr. Grinch"] = "You're A Mean One Mr. Grinch"
songs$Artist[songs$Artist == "Ke$ha"] = "Kesha"
songs$Track[songs$Track == "Jump - 2015 Remaster"] = "Jump"
songs$Track[songs$Track == "When Iâ€™m Gone (with Katy Perry)"] = "When I'm Gone (with Katy Perry)"
songs$Track[songs$Track == "Cups (Pitch Perfectâ€™s â€œWhen Iâ€™m Goneâ€)"] = "Cups"
songs$Track[songs$Track == "Cups (Pitch Perfectâ€™s â€œWhen Iâ€™m Goneâ€) - Pop Version"] = "Cups"
songs$Track[songs$Track == "Youâ€™ll Never Know"] = "You'll Never Know"
songs$Track[songs$Track == "Dancingâ€™s Done"] = "Dancing's Done"
songs$Track[songs$Track == "Maybe Youâ€™re the Problem"] = "Maybe You're the Problem"
songs$Artist[songs$Artist == "BeyoncÃ©"] = "Beyonce"
songs$Artist[songs$Artist == "CÃ©line Dion"] = "Celine Dion"
songs$Track[songs$Track == "I Didnâ€™t Just Come Here To Dance"] = "I Didn't Just Come Here To Dance"
songs$Track[songs$Track == "Letâ€™s Get Lost"] = "Let's Get Lost"
songs$Track[songs$Track == "Love Again"] = "Love Again - Bonus Track"
songs$Track[songs$Track == "Tonight Iâ€™m Getting Over You"] = "Tonight I'm Getting Over You"
songs$Track[songs$Track == "Tonight Iâ€™m Getting Over You - Remix"] = "Tonight I'm Getting Over You - Remix"
songs$Artist[songs$Artist == "Carolina GaitÃ¡n - La Gaita"] = "Carolina Gaitan - La Gaita"
songs$Track[songs$Track == "Canâ€™t Help Falling In Love - The Voice Performance"] = "Can't Help Falling In Love - The Voice Performance"
songs$Track[songs$Track == "Hold On Weâ€™re Going Home - The Voice Performance"] = "Hold On We're Going Home - The Voice Performance"
songs$Track[songs$Track == "I Wonâ€™t Give Up - The Voice Performance"] = "I Won't Give Up - The Voice Performance"
songs$Track[songs$Track == "Listen To Your Heart (EdmÃ©e's Unplugged Vocal)"] = "Listen To Your Heart (Edmee's Unplugged Vocal)"
songs$Track[songs$Track == "Cake By The Ocean - ã‚¯ãƒªãƒ¼ãƒ³ãƒ»ãƒ´ã‚¡ãƒ¼ã‚¸ãƒ§ãƒ³"] = "Cake by the Ocean"
songs$Track[songs$Track == "If It Ainâ€™t Me"] = "If It Ain't Me"
songs$Track[songs$Track == "Wildflowers - From â€œTeen Spiritâ€ Soundtrack"] = "Wildflowers"
songs$Artist[songs$Artist == "Elley DuhÃ©"] = "Elley Duhe"
songs$Track[songs$Track == "Mirror - From â€œThe Hunger Games: Catching Fireâ€ Soundtrack"] = "Mirror"
songs$Artist[songs$Artist == "KÃ¤Ã¤rijÃ¤"] = "Kaarija"
songs$Track[songs$Track == "Santa Canâ€™t You Hear Me"] = "Santa Can't You Hear Me"
songs$Track[songs$Track == "i donâ€™t wanna dieâ€¦"] = "i don't wanna die"
songs$Track[songs$Track == "Tell Me Itâ€™s A Nightmare"] = "Tell Me It's a Nightmare"
songs$Track[songs$Track == "It Ainâ€™t Me (with Selena Gomez)"] = "It Ain't Me (with Selena Gomez)"
songs$Track[songs$Track == "Ghostbusters - from Ghostbusters"] = "Ghostbusters"
songs$Track[songs$Track == "Take My Breath - Single Version"] = "Take My Breath"
songs$Track[songs$Track == "Tie Me Down (with Elley DuhÃ©)"] = "Tie Me Down (with Elley Duhe)"
songs$Track[songs$Track == "Dirrty (feat. Redman)"] = "Dirrty"
songs$Track[songs$Track == "Silver And Gold - From Rudolph The Red-Nosed Reindeer Soundtrack"] = "Silver and Gold"
songs$Track[songs$Track == "Rockstar - Original Version"] = "Rock Star"
songs$Track[songs$Track == "Somebody Find Me (feat. Kait Weston)"] = "Somebody Find Me"
songs$Track[songs$Track == "Luv Me A Little (feat. Nina Nesbitt)"] = "Luv Me a Little"

#Changing Search Song Title
songs$Track[songs$Track == "Respect - Remastered"] = "Respect"
songs$Track[songs$Track == "Respect - Original Version"] = "Respect"
songs$Track[songs$Track == "Mony Mony - 24-Bit Digitally Remastered 01"] = "Mony Mony"


distincts = songs %>% 
  group_by(Artist,Track) %>%
  summarize(streams = n())

qualifying = distincts %>% 
  filter(streams >= 5) %>%
  filter(Artist != "Amy Guess") %>% #Doesn't Exist
  filter(Track != "Almost Said It") %>% #Not in US Market
  filter(Track != "Anyone Of Us (Stupid Mistake)") %>%
  filter(Artist != "f(x)") %>%
  filter(Track != "Hawaiian Roller Coaster Ride - From Lilo & Stitch/Soundtrack Version") %>% #Causing problems for lookup
  filter(Artist != "Rachel Bloom") %>% #Comedy
  filter(Artist != "Rockie Gold") %>% #Comedy
  filter(Track != "Dancing Lasha Tumbai") %>% #Comedy
  filter(Track != "We're the Same") %>% #Can't Find It
  filter(Artist != "Crazy Ex-Girlfriend Cast") %>% #Comedy
  filter(Artist != "Anderson") %>% #Causing Problems... Christmas Song - May not be available
  filter(Artist != "Kathleen Madigan") %>% #Comedy
  filter(Track != "Oh Holy Night") %>% #Not available
  filter(Track != "My Grown Up Christmas List") %>% #Not available
  filter(Track != "Youniverse") #Not available

qsongs = songs %>% 
  inner_join(.,qualifying,by=c("Artist","Track")) %>% 
  select(-c(streams)) 


#Create Modified List for Spotify Lookup

#Change Artist Name For Lookup ---> Problem with escape characters in string
qualifying$Artist[qualifying$Artist == "Axwell /\\ Ingrosso"] = "Axwell Ingrosso"
qualifying$Artist[qualifying$Artist == "Aly & AJ"] = "Aly AJ"
qualifying$Artist[qualifying$Artist == "Earth Wind & Fire"] = "Earth Wind Fire"

song_lookup = qualifying %>%
  mutate(temp=paste(Artist,Track,Sep=" ")) %>%
  mutate(temp2=gsub("'","",temp)) %>%
  mutate(Lookup=gsub(" ","%20",temp2)) %>%
  select(-c(temp,temp2))
song_lookup$id = NA
song_lookup$artist_id = NA

#Change Artist Name Back
qualifying$Artist[qualifying$Artist == "Axwell Ingrosso"] = "Axwell /\\ Ingrosso"
song_lookup$Artist[song_lookup$Artist == "Axwell Ingrosso"] = "Axwell /\\ Ingrosso"
qualifying$Artist[qualifying$Artist == "Aly AJ"] = "Aly & AJ"
song_lookup$Artist[song_lookup$Artist == "Aly AJ"] = "Aly & AJ"
qualifying$Artist[qualifying$Artist == "Earth Wind Fire"] = "Earth Wind & Fire"
song_lookup$Artist[song_lookup$Artist == "Earth Wind Fire"] = "Earth Wind & Fire"

#Change Lookup String
song_lookup$Lookup[song_lookup$Lookup == "Britt%20Nicole%20Gold%20-%20Jason%20Nevins%20Rhythmic%20Remix/Bonus%20Track%20%20"] = "Britt%20Nicole%20Gold%20Jason%20Nevins"
song_lookup$Lookup[song_lookup$Lookup == "Carly%20Rae%20Jepsen%20I%20Didnt%20Just%20Come%20Here%20To%20Dance%20%20"] = "Carly%20Rae%20Jepsen%20I%20Just%20Come%20Here%20To%20Dance"
song_lookup$Lookup[song_lookup$Lookup == "Carly%20Rae%20Jepsen%20Lets%20Get%20Lost%20%20"] = "Carly%20Rae%20Jepsen%20Get%20Lost"
song_lookup$Lookup[song_lookup$Lookup == "Celine%20Dion%20Beauty%20and%20the%20Beast%20-%20Duet%20with%20Peabo%20Bryson%20from%20the%20Soundtrack%20Beauty%20and%20the%20Beast%20%20"] = "Celine%20Dion%20Beauty%20and%20the%20Beast"
song_lookup$Lookup[song_lookup$Lookup == "Christina%20Grimmie%20Cant%20Help%20Falling%20In%20Love%20-%20The%20Voice%20Performance%20%20"] = "Christina%20Grimmie%20Help%20Falling%20In%20Love"
song_lookup$Lookup[song_lookup$Lookup == "Christina%20Grimmie%20Hold%20On%20Were%20Going%20Home%20-%20The%20Voice%20Performance%20%20"] = "Christina%20Grimmie%20Hold%20On%20Going%20Home%20-%20The%20Voice%20Performance%20%20"
song_lookup$Lookup[song_lookup$Lookup == "David%20Guetta%20She%20Wolf%20(Falling%20to%20Pieces)%20[feat.%20Sia]%20%20"] = "David%20Guetta%20She%20Wolf"
song_lookup$Lookup[song_lookup$Lookup == "Dead%20or%20Alive%20You%20Spin%20Me%20Round%20(Like%20a%20Record)%20%20"] = "Dead%20or%20Alive%20You%20Spin%20Me%20Round"
song_lookup$Lookup[song_lookup$Lookup == "Def%20Leppard%20Pour%20Some%20Sugar%20On%20Me%20(2012)%20%20"] = "Def%20Leppard%20Pour%20Some%20Sugar%20On%20Me"
song_lookup$Lookup[song_lookup$Lookup == "Eurythmics%20Sisters%20Are%20Doin%20It%20for%20Themselves%20(Than%20Never%20to%20Have%20Loved)%20%20"] = "Eurythmics%20Sisters%20Are%20It%20for%20Themselves"
song_lookup$Lookup[song_lookup$Lookup == "Fergie%20M.I.L.F.%20$%20%20"] = "Fergie%20M.I.L.F."
song_lookup$Lookup[song_lookup$Lookup == "Fergie%20L.A.LOVE%20(la%20la)%20%20"] = "Fergie%20L.A.LOVE"
song_lookup$Lookup[song_lookup$Lookup == "Gareth%20Emery%20Saving%20Light%20(feat.%20Haliene)%20%20"] = "Gareth%20Emery%20Saving%20Light"
song_lookup$Lookup[song_lookup$Lookup == "Gayla%20Peevey%20I%20Want%20a%20Hippopotamus%20for%20Christmas%20(Hippo%20the%20Hero)%20%20"] = "Gayla%20Peevey%20I%20Want%20a%20Hippopotamus%20for%20Christmas"
song_lookup$Lookup[song_lookup$Lookup == "Jason%20Weaver%20I%20Just%20Cant%20Wait%20To%20Be%20King%20-%20From%20The%20Lion%20King/Soundtrack%20Version%20%20	"] = "Jason%20Weaver%20I%20Just%20Wait%20To%20Be%20King"
song_lookup$Lookup[song_lookup$Lookup == "Jermaine%20Stewart%20We%20Dont%20Have%20To%20Take%20Our%20Clothes%20Off%20-%207%20Version%20%20"] = "Jermaine%20Stewart%20We%20Dont%20Have%20To%20Take%20Our%20Clothes%20Off"
song_lookup$Lookup[song_lookup$Lookup == "John%20Lennon%20Happy%20Xmas%20(War%20Is%20Over)%20-%202010%20Digital%20Remaster%20%20"] = "John%20Lennon%20Happy%20Xmas"
song_lookup$Lookup[song_lookup$Lookup == "John%20Parr%20St.%20Elmos%20Fire%20(Man%20in%20Motion)%20%20"] = "John%20Parr%20St.%20Elmos%20Fire"
song_lookup$Lookup[song_lookup$Lookup == "Josh%20Groban%20All%20I%20Ask%20Of%20You%20(duet%20with%20Kelly%20Clarkson)%20-%20From%20The%20Phantom%20Of%20The%20Opera%20%20"] = "Josh%20Groban%20All%20I%20Ask%20Of%20You"
song_lookup$Lookup[song_lookup$Lookup == "Josh%20Groban%20Angels%20We%20Have%20Heard%20On%20High%20-%20Duet%20With%20Brian%20Mcknight%20%20"] = "Josh%20Groban%20Angels%20We%20Have%20Heard%20On%20High"
song_lookup$Lookup[song_lookup$Lookup == "Josh%20Groban%20O%20Come%20All%20Ye%20Faithful%20(with%20The%20Mormon%20Tabernacle%20Choir%20under%20the%20direction%20of%20Craig%20Jessop)%20%20"] = "Josh%20Groban%20O%20Come%20All%20Ye%20Faithful"
song_lookup$Lookup[song_lookup$Lookup == "Josh%20Groban%20The%20First%20Noel%20-%20Duet%20With%20Faith%20Hill%20%20"] = "Josh%20Groban%20The%20First%20Noel"
song_lookup$Lookup[song_lookup$Lookup == "Katy%20Perry%20California%20Gurls%20(feat.%20Snoop%20Dogg)%20%20"] = "Katy%20Perry%20California%20Gurls"
song_lookup$Lookup[song_lookup$Lookup == "Genesis%20Invisible%20Touch%20-%202007%20Remastered%20Version%20%20"] = "Genesis%20Invisible%20Touch"
song_lookup$Lookup[song_lookup$Lookup == "Havana%20Brown%20We%20Run%20The%20Night%20-%20Edited%20%20"] = "Havana%20Brown%20We%20Run%20The%20Night"
song_lookup$Lookup[song_lookup$Lookup == "Idina%20Menzel%20Let%20It%20Go%20-%20From%20Frozen/Soundtrack%20Version%20%20"] = "Idina%20Menzel%20Let%20It%20Go"
song_lookup$Lookup[song_lookup$Lookup == "Hannah%20Montana%20Rockstar%20-%20Original%20Version%20%20"] = "Hannah%20Montana%20Rockstar"
song_lookup$Lookup[song_lookup$Lookup == "Macklemore%20&%20Ryan%20Lewis%20Thrift%20Shop%20(feat.%20Wanz)%20%20"] = "Macklemore%20&%20Ryan%20Lewis%20Thrift%20Shop"
song_lookup$Lookup[song_lookup$Lookup == "Mako%20I%20Wont%20Let%20You%20Walk%20Away%20-%20Radio%20Edit%20%20"] = "Mako%20I%20Let%20You%20Walk%20Away"
song_lookup$Lookup[song_lookup$Lookup == "Mako%20I%20Wont%20Let%20You%20Walk%20Away%20-%20Lost%20Kings%20Remix%20%20"] = "Mako%20I%20Let%20You%20Walk%20Away%20Lost%20Kings"
song_lookup$Lookup[song_lookup$Lookup == "Mannheim%20Steamroller%20God%20Rest%20Ye%20Merry%20Gentlemen%20-%20Rock%20Version%20%20"] = "Mannheim%20Steamroller%20God%20Rest%20Ye%20Merry%20Gentlemen%20"
song_lookup$Lookup[song_lookup$Lookup == "Mannheim%20Steamroller%20Lo%20How%20A%20Rose%20Eer%20Blooming%20%20"] = "Mannheim%20Steamroller%20Lo%20How%20A%20Rose%20Blooming%20%20"
song_lookup$Lookup[song_lookup$Lookup == "Owl%20City%20Good%20Time%20-%20Wideboys%20Remix%20[Club]%20%20"] = "Owl%20City%20Good%20Time%20-%20Wideboys%20Remix"
song_lookup$Lookup[song_lookup$Lookup == "Matt%20Redman%2010000%20Reasons%20(Bless%20the%20Lord)%20-%20Live%20%20"] = "Matt%20Redman%20Reasons"
song_lookup$Lookup[song_lookup$Lookup == "Phil%20Collins%20You%20Cant%20Hurry%20Love%20-%202016%20Remastered%20%20"] = "Phil%20Collins%20You%20Cant%20Hurry%20Love"
song_lookup$Lookup[song_lookup$Lookup == "Pierce%20Fulton%20Kuaga%20(Lost%20Time)%20(Radio%20Edit)%20%20"] = "Pierce%20Fulton%20Kuaga"
song_lookup$Lookup[song_lookup$Lookup == "Queen%20Dont%20Stop%20Me%20Now%20-%20Remastered%20%20"] = "Queen%20Dont%20Stop%20Me%20Now"
song_lookup$Lookup[song_lookup$Lookup == "Samuel%20E.%20Wright%20Under%20The%20Sea%20-%20From%20The%20Little%20Mermaid/%20Soundtrack%20Version%20%20"] = "Samuel%20Wright%20Under%20The%20Sea"
song_lookup$Lookup[song_lookup$Lookup == "Rachel%20Platten%201000%20Ships%20%20"] = "Rachel%20Platten%20Ships"
song_lookup$Lookup[song_lookup$Lookup == "Red%20One%20Dont%20You%20Need%20Somebody%20(feat.%20Enrique%20Iglesias%20R.%20City%20Serayah%20&%20Shaggy)%20%20"] = "RedOne%20You%20Need%20Somebody"
song_lookup$Lookup[song_lookup$Lookup == "Rihanna%20Love%20The%20Way%20You%20Lie%20(Part%20II)%20-%20Pt.%202%20%20"] = "Rihanna%20Love%20The%20Way%20You%20Lie"
song_lookup$Lookup[song_lookup$Lookup == "Raven-Symone%20Thats%20So%20Raven%20(Theme%20Song)%20%20"] = "Raven-Symone%20So%20Raven"
song_lookup$Lookup[song_lookup$Lookup == "Rufus%20The%20Naked%20Mole%20Rap%20-%20From%20Kim%20Possible%20%20"] = "Rufus%20The%20Naked%20Mole%20Rap"
song_lookup$Lookup[song_lookup$Lookup == "Susan%20Egan%20I%20Wont%20Say%20(Im%20In%20Love)%20%20"] = "Susan%20Egan%20I%20Wont%20Say"
song_lookup$Lookup[song_lookup$Lookup == "Nicole%20Scherzinger%20Dont%20Hold%20Your%20Breath"] = "Nicole%20Scherzinger%20Hold%20Your%20Breath"
song_lookup$Lookup[song_lookup$Lookup == "The%20Barden%20Bellas%20Bellas%20Finals:%20Price%20Tag/Dont%20You%20(Forget%20About%20Me)/Give%20Me%20Everything/Just%20The%20Way%20You%20Are/Party%20In%20The%20U.S.A./Turn%20The%20Beat%20Around%20%20"] = "The%20Barden%20Bellas%20Bellas%20Finals"
song_lookup$Lookup[song_lookup$Lookup == "The%20Barden%20Bellas%20Bellas%20Regionals:%20The%20Sign/Eternal%20Flame/Turn%20The%20Beat%20Around%20%20"] = "The%20Barden%20Bellas%20Bellas%20Regionals"
song_lookup$Lookup[song_lookup$Lookup == "The%20Barden%20Bellas%20Cups%20(When%20Im%20Gone)%20-%20Campfire%20Version%20%20"] = "The%20Barden%20Bellas%20Cups"
song_lookup$Lookup[song_lookup$Lookup == "The%20Barden%20Bellas%20Pool%20Mashup:%20Just%20The%20Way%20You%20Are/Just%20A%20Dream%20%20"] = "The%20Barden%20Bellas%20Pool"
song_lookup$Lookup[song_lookup$Lookup == "The%20Disney%20Afternoon%20Studio%20Chorus%20Chip%20N%20Dales%20Rescue%20Rangers%20Theme%20Song%20-%20From%20Chip%20n%20Dales%20Rescue%20Rangers%20%20"] = "The%20Disney%20Afternoon%20Studio%20Chorus%20Chip%20N%20Dales%20Rescue%20Rangers"
song_lookup$Lookup[song_lookup$Lookup == "The%20Disney%20Afternoon%20Studio%20Chorus%20Duck%20Tales%20Theme%20-%20From%20Duck%20Tales%20%20"] = "The%20Disney%20Afternoon%20Studio%20Chorus%20Duck%20Tales"
song_lookup$Lookup[song_lookup$Lookup == "The%20J.%20Geils%20Band%20Centerfold%20-%202006%20-%20Remaster%20%20"] = "The%20J.%20Geils%20Band%20Centerfold"
song_lookup$Lookup[song_lookup$Lookup == "Thomas%20Helmore%20A%20Christmas%20Festival%20Medley:%20Joy%20To%20The%20World%20/%20Deck%20The%20Halls%20/%20Good%20King%20Wenceslas%20/%20God%20Rest%20Ye%20Merry%20Gentlemen%20/%20Hark!%20The%20Herald%20Angels%20Sing%20/%20The%20First%20Noel%20/%20Silent%20Night%20/%20Jingle%20Bells%20/%20O%20Come%20All%20Ye%20Faithful%20%20"] = "Thomas%20Helmore%20A%20Christmas%20Festival"
song_lookup$Lookup[song_lookup$Lookup == "Tori%20Kelly%20Colors%20Of%20The%20Wind%20-%20From%20Pocahontas%20%20"] = "Tori%20Kelly%20Colors%20Of%20The%20Wind"
song_lookup$Lookup[song_lookup$Lookup == "Two%20Friends%20Brighter%20(feat.%20Jeff%20Sontag%20&%20I%20Am%20Lightyear)%20[Radio%20Edit]%20%20"] = "Two%20Friends%20Brighter"
song_lookup$Lookup[song_lookup$Lookup == "tyDi%20Redefined%20(feat.%20Melanie%20Fontana%20&%20Novaspace)%20[Club%20Edit]%20%20"] = "tyDi%20Redefined"
song_lookup$Lookup[song_lookup$Lookup == "Van%20Halen%20Jump%20-%202015%20Remastered%20Version%20%20"] = "Van%20Halen%20Jump"
song_lookup$Lookup[song_lookup$Lookup == "Van%20McCoy%20The%20Hustle%20-%20Original%20Mix%20%20"] = "Van%20McCoy%20The%20Hustle"
song_lookup$Lookup[song_lookup$Lookup == "The%20Chainsmokers%20Takeaway%20(with%20ILLENIUM%20&%20Lennon%20Stella)%20%20"] = "The%20Chainsmokers%20Takeaway"
song_lookup$Lookup[song_lookup$Lookup == "Vigiland%20Were%20the%20Same%20%20"] = "Vigiland%20Same"
song_lookup$Lookup[song_lookup$Lookup == "Vince%20Guaraldi%20Trio%20Christmas%20Time%20Is%20Here%20-%20Vocal%20%20"] = "Vince%20Guaraldi%20Trio%20Christmas%20Time%20Is%20Here"
song_lookup$Lookup[song_lookup$Lookup == "Whitney%20Houston%20I%20Wanna%20Dance%20with%20Somebody%20(Who%20Loves%20Me)%20%20"] = "Whitney%20Houston%20I%20Wanna%20Dance%20with%20Somebody"
song_lookup$Lookup[song_lookup$Lookup == "Whitney%20Houston%20The%20Star%20Spangled%20Banner%20-%20Live%20from%20Super%20Bowl%20XXV%20%20"] = "Whitney%20Houston%20The%20Star%20Spangled%20Banner"
song_lookup$Lookup[song_lookup$Lookup == "Zara%20Larsson%20Aint%20My%20Fault%20%20"] = "Zara%20Larsson%20My%20Fault%20%20"
song_lookup$Lookup[song_lookup$Lookup == "Earth%20Wind%20Fire%20Lets%20Groove%20%20"] = "Earth%20Wind%20Fire%20Groove%20%20"
song_lookup$Lookup[song_lookup$Lookup == "Dua%20Lipa%20Dont%20Start%20Now%20%20"] = "Dua%20Lipa%20Start%20Now%20%20"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20All%20You%20Need%20To%20Know%20(feat.%20Calle%20Lehmann)%20%20"] = "Gryffin%20All%20You%20Need%20To%20Know"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20Baggage%20(with%20AlunaGeorge)%20%20"] = "Gryffin%20Baggage"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20Body%20Back%20(feat.%20Maia%20Wright)%20%20"] = "Gryffin%20Body%20Back"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20Bye%20Bye%20(feat.%20Ivy%20Adara)%20%20"] = "Gryffin%20Bye%20Bye"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20If%20I%20Left%20The%20World%20(feat.%20MARINA%20&%20Model%20Child)%20%20"] = "Gryffin%20If%20I%20Left%20The%20World"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20Just%20For%20A%20Moment%20(feat.%20Iselin)%20%20"] = "Gryffin%20Just%20For%20A%20Moment"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20Need%20Your%20Love%20(with%20Noah%20Kahan)%20%20"] = "Gryffin%20Need%20Your%20Love"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20Nobody%20Compares%20to%20You%20(feat.%20Katie%20Pearlman)%20%20"] = "Gryffin%20Nobody%20Compares%20to%20You"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20OMG%20(with%20Carly%20Rae%20Jepsen)%20%20"] = "Gryffin%20OMG"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20OMG%20(with%20Carly%20Rae%20Jepsen)%20-%20Anki%20Remix%20%20"] = "Gryffin%20OMG%20Anki%20Remix"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20Tie%20Me%20Down%20(with%20Elley%20DuhÃÂ©)%20%20"] = "Gryffin%20Tie%20Me%20Down"
song_lookup$Lookup[song_lookup$Lookup == "Gryffin%20You%20Remind%20Me%20(feat.%20Stanaj)%20%20"] = "Gryffin%20You%20Remind%20Me"
song_lookup$Lookup[song_lookup$Lookup == "Elle%20Fanning%20Wildflowers%20-%20From%20Ã¢???oTeen%20SpiritÃ¢???Â%20Soundtrack%20%20"] = "Elle%20Fanning%20Wildflowers"
song_lookup$Lookup[song_lookup$Lookup == "Lana%20Del%20Rey%20Summertime%20Sadness%20(Lana%20Del%20Rey%20Vs.%20Cedric%20Gervais)%20-%20Cedric%20Gervais%20Remix%20/%20Radio%20Edit%20%20"] = "Lana%20Del%20Rey%20Summertime%20Sadness%20Cedric%20Gervais%20Remix"
song_lookup$Lookup[song_lookup$Lookup == "Owl%20City%20Good%20Time%20-%20Wideboys%20Remix%20[Club]%20%20"] = "Owl%20City%20Good%20Time%20Wideboys%20Remix%20"
song_lookup$Lookup[song_lookup$Lookup == "Two%20Friends%20Brighter%20(feat.%20Jeff%20Sontag%20&%20I%20Am%20Lightyear)%20[Radio%20Edit]%20%20"] = "Two%20Friends%20Brighter%20(feat.%20Jeff%20Sontag%20&%20I%20Am%20Lightyear)%20%20"
song_lookup$Lookup[song_lookup$Lookup == "tyDi%20Redefined%20(feat.%20Melanie%20Fontana%20&%20Novaspace)%20[Club%20Edit]%20%20"] = "tyDi%20Redefined%20(feat.%20Melanie%20Fontana%20&%20Novaspace)%20Club%20Edit%20%20"
song_lookup$Lookup[song_lookup$Lookup == "GATTÃœSO%20When%20In%20Rome%20-%20Steve%20Brian%20Remix%20%20"] = "GATTUSO%20When%20In%20Rome%20-%20Steve%20Brian%20Remix%20%20"

#USE LOOKUP FIELD TO HIT SPOTIFY API SERVER
# Can get OAuth_Token at: https://developer.spotify.com/console/get-search-item/?q=Carly%20Rae%20Jepsen%20Run%20Away%20With%20Me&type=track&market=&limit=1&offset=

OAuth_Token = "BQBRku3D19nljwDBf0eRQL4MTbvTQlAj8jy2qmxnzYNowhdgdelP_5nU5sDhLYvIUwrdYIlvwH42n3kscSVC5yrG5JuPO-fWItYIxfIZ_a2DqOfgpByPdjyswDzK5ipn3jLF2ctKpNwU9Xg4RxyGFs4OxzNrulzZFnWCpykuNoI89N2AX7u5HQ"

for(i in seq(from=1,to=dim(song_lookup)[1],by=1)){
  cat(i,"\n",sep="")
  theLookup = song_lookup[i,"Lookup"]
  temp = glue('\'curl -X "GET" "https://api.spotify.com/v1/search?q={theLookup}&type=track&limit=1" -H "Accept: application/json" -H "Content-Type: application/json" -H "Authorization: Bearer {OAuth_Token}"\'')
  song_lookup[i,"id"] = fromJSON(system(substr(temp,2,nchar(temp)-1),inter=T,ignore.stderr = T))$tracks$items$id
  song_lookup[i,"artist_id"] = fromJSON(system(substr(temp,2,nchar(temp)-1),inter=T,ignore.stderr = T))$tracks$items$artists[[1]][1,"id"]
}

#Manual Override - mapping to wrong id (such as title tracks of albums or studio v live version)
#CHECK - No Duplicates
song_lookup[song_lookup$Artist=="Carly Rae Jepsen" & song_lookup$Track=="Emotion","id"] = "6f8dVDn1vWbWIfUouv9iJp"
song_lookup[song_lookup$Artist=="Carrie Underwood" & song_lookup$Track=="Some Hearts","id"] = "4VsMvUldyABTPUodYRI6uX"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="Halcyon","id"] = "5f1BYfuMovgQPuYPIGfESE"
song_lookup[song_lookup$Artist=="Jessie J" & song_lookup$Track=="Who You Are","id"] = "65lIGCfW59BxcEJnnIOCq3"
song_lookup[song_lookup$Artist=="Karmin" & song_lookup$Track=="Pulses","id"] = "7lHkU78B86mdBbU0WIVswi"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Breakaway","id"] = "61Qhe2mHSLhUE04QeK4lkD"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Piece by Piece","id"] = "2NELtMgQ8HSdrGrYQPLnC3"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Wrapped in Red","id"] = "5fclVBnzaGrbucvVMC228o"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="The Fame","id"] = "25pWYL5848CIMh3OfnDroM"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="Monster","id"] = "1K9jK8EHkgOTjLokIbIxPq"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="Born This Way","id"] = "6r2BECwMgEoRb5yLfp0Hca"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="ARTPOP","id"] = "73zDQykFwu3yT5VQ6MwYbh"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="Joanne","id"] = "2kecdnlyueotEMC8rdNlf6"
song_lookup[song_lookup$Artist=="Mako" & song_lookup$Track=="I Won't Let You Walk Away - Radio Edit","id"] = "02luOsVJEhcVH5Zf3n7Gq6"
song_lookup[song_lookup$Artist=="Molly Sanden" & song_lookup$Track=="Like No One's Watching","id"] = "0seWbF1zqWCa0dkMupdd0S"
song_lookup[song_lookup$Artist=="The Barden Bellas" & song_lookup$Track=="Party In The U.S.A.","id"] = "7c2KF18pPpaN8Hy5MiNNWs"
song_lookup[song_lookup$Artist=="Cardiknox" & song_lookup$Track=="On My Way","id"] = "5spSDPY097Y2PwXQYIFyZb"
song_lookup[song_lookup$Artist=="Selena Gomez & the Scene" & song_lookup$Track=="Naturally","id"] = "1YaVmBh7EAeR54FIjuFcb5"
song_lookup[song_lookup$Artist=="Selena Gomez & the Scene" & song_lookup$Track=="Hit the Lights","id"] = "6ciLLh4TkPYDUfNxn2Z0KO"
song_lookup[song_lookup$Artist=="Elmo & Patsy" & song_lookup$Track=="Grandma Got Run Over By A Reindeer","id"] = "49iHYFjT5yO6WEw6KerX9o"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Beautiful Disaster","id"] = "6bcnBvXEsaqFxO5jkgwPwR"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Gone","id"] = "5Canmwvufsc7bMM3v2c286"
song_lookup[song_lookup$Artist=="Kool & The Gang" & song_lookup$Track=="Celebration - Single Version","id"] = "3K7Q9PHUWPTaknlbFPThn2"
song_lookup[song_lookup$Artist=="Kool & The Gang" & song_lookup$Track=="Get Down On It","id"] = "4yKZACkuudvfd600H2dQie"
song_lookup[song_lookup$Artist=="Vigiland" & song_lookup$Track=="Shots & Squats - SoMo Vocal Mix","id"] = "40l5cmtb4BpqwNxwrQZkt8"
song_lookup[song_lookup$Artist=="Sia" & song_lookup$Track=="Move Your Body","id"] = "73Mfgc0HTZeNCoNrDI46sJ"
song_lookup[song_lookup$Artist=="Ryn Weaver" & song_lookup$Track=="The Fool","id"] = "5VF6OStYFWd5QTpxTvDXCU"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="Dope","id"] = "37yshWNelgzYqQunLz4hEL"
song_lookup[song_lookup$Artist=="Stevie Wonder" & song_lookup$Track=="What Christmas Means to Me","id"] = "3h1EREJfCwyv6cG7CGak5d"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="Chromatica I","id"] = "3CGZ7wfk4skmuyQgua1C1K"
song_lookup[song_lookup$Artist=="Owl City" & song_lookup$Track=="Good Time - Wideboys Remix [Club]","id"] = "2LflnXoxOqnQGnC1GoYpaZ"
song_lookup[song_lookup$Artist=="Gryffin" & song_lookup$Track=="OMG (with Carly Rae Jepsen) - Anki Remix","id"] = "4BXXIMSAkSfXaDHmSudKZw"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Respect - Original Version","id"] = "4sIEifoZXsJOVRYDPaNDKp"
song_lookup[song_lookup$Artist=="Lucas & Steve" & song_lookup$Track=="All Cried Out","id"] = "4QgOLpoO7pzgcVn89H4VtP"
song_lookup[song_lookup$Artist=="Lucas & Steve" & song_lookup$Track=="I Want It All","id"] = "3oSCJeOl0XjcpNcigO9vj7"
song_lookup[song_lookup$Artist=="The Knocks" & song_lookup$Track=="Love Me Like That (feat. Carly Rae Jepsen) - The Knocks 55.5 VIP Mix","id"] = "0hhO1Hn3lzMYInmVhyNOMa"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="Goodness Gracious","id"] = "2RW9qqrhPwMPHlWx57CalB"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Baby It's Cold Outside","id"] = "07D8WWheVdl0xZ4YzOcfan"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Baby It's Cold Outside","artist_id"] = "3BmGtnKgCSGYIUhmivXKWX"
song_lookup[song_lookup$Artist=="Billy Joel" & song_lookup$Track=="Why Should I Worry","artist_id"] = "6zFYqv1mOsgBRQbae3JJ9e"
song_lookup[song_lookup$Artist=="Bruce Springsteen" & song_lookup$Track=="Santa Claus Is Comin' To Town - Single Version","id"] = "6s2wpWPFPAgKg2LXxi1Oee"
song_lookup[song_lookup$Artist=="Bruce Springsteen" & song_lookup$Track=="Santa Claus Is Comin' To Town - Single Version","artist_id"] = "3eqjTLE0HfPfh78zjh6TqT"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="Power","id"] = "6smYfKpqsvpmqBXkLjYGJo"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="Power","artist_id"] = "0X2BH1fck6amBIoJhDVmmJ"
song_lookup[song_lookup$Artist=="Jim Yosef" & song_lookup$Track=="This Time","artist_id"] = "40HDiLfKm0tXk2FxlJx6aO"
song_lookup[song_lookup$Artist=="MGM Studio Chorus" & song_lookup$Track=="Welcome Christmas","artist_id"] = "2EjrRJZBrUhYqcgmYF69N1"
song_lookup[song_lookup$Artist=="Molly Sanden" & song_lookup$Track=="Youniverse","id"] = "3QCHHJh6kepIINXylSxDjl"
song_lookup[song_lookup$Artist=="Molly Sanden" & song_lookup$Track=="Youniverse","artist_id"] = "0NRMzT05nsc8mTm4iUvuHY"
song_lookup[song_lookup$Artist=="Rihanna" & song_lookup$Track=="Love The Way You Lie (Part II) - Pt. 2","id"] = "4aPRuTsiG7B4owTTF8Dm3U"
song_lookup[song_lookup$Artist=="Rihanna" & song_lookup$Track=="Love The Way You Lie (Part II) - Pt. 2","artist_id"] = "5pKCCKE2ajJHZ9KAiaK11H"
song_lookup[song_lookup$Artist=="Rita Ora" & song_lookup$Track=="R.I.P.","id"] = "7CnOtdsHghoRY8f5bqUBaM"
song_lookup[song_lookup$Artist=="Rita Ora" & song_lookup$Track=="R.I.P.","artist_id"] = "5CCwRZC6euC8Odo6y9X8jr"
song_lookup[song_lookup$Artist=="Ariana Grande" & song_lookup$Track=="You'll Never Know","id"] = "6d8XhbAw4IoKPnVD07Sb9b"
song_lookup[song_lookup$Artist=="Ava Max" & song_lookup$Track=="EveryTime I Cry","id"] = "0mV43B6pJWRjcM5TmzNe6d"
song_lookup[song_lookup$Artist=="Dua Lipa" & song_lookup$Track=="Physical","id"] = "3AzjcOeAmA57TIOr9zF1ZW"
song_lookup[song_lookup$Artist=="Dua Lipa" & song_lookup$Track=="Hallucinate","id"] = "1nYeVF5vIBxMxfPoL0SIWg"
song_lookup[song_lookup$Artist=="Dua Lipa" & song_lookup$Track=="Be the One","id"] = "7FCfMXYTIiQ9b4hDYs4Iol"
song_lookup[song_lookup$Artist=="Earth Wind & Fire" & song_lookup$Track=="September","id"] = "2grjqo0Frpf2okIBiifQKs"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="By the End of the Night","id"] = "7Fqlku15iPyQyNuxU0I8rY"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="Like a Saviour","id"] = "5EMwXfQMEyw9CvzQAb0N7G"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="Let It Die","id"] = "3bbz8RABvRCGO3s4mDvoFw"
song_lookup[song_lookup$Artist=="Ellie Goulding" & song_lookup$Track=="Easy Lover (feat. Big Sean)","id"] = "0PXXVEQZ3E24vc6XqHAqz4"
song_lookup[song_lookup$Artist=="Jess Glynne" & song_lookup$Track=="All I Am","id"] = "6X6SdJzxyw7GzxwP2fr9WV"
song_lookup[song_lookup$Artist=="Marina" & song_lookup$Track=="Power & Control","id"] = "2IPMpbBwyCm2V6azTZgAKj"
song_lookup[song_lookup$Artist=="Taylor Swift" & song_lookup$Track=="Wildest Dreams","id"] = "106R7Z57WYzBAfrXImV30y"
song_lookup[song_lookup$Artist=="Taylor Swift" & song_lookup$Track=="This Love","id"] = "1kTPQnabROVkW9bUXdCGrB"
song_lookup[song_lookup$Artist=="The Weeknd" & song_lookup$Track=="Take My Breath","id"] = "2vgUijXOTRMnWXDtvgMp2b"
song_lookup[song_lookup$Artist=="Lope & Kantola" & song_lookup$Track=="Words Can't Say","id"] = "1Dvtje0LRJogRjMV9hLqAP"
song_lookup[song_lookup$Artist=="Lope & Kantola" & song_lookup$Track=="Words Can't Say","artist_id"] = "2gy6mlx7uNGaZSsohHQhKX"
song_lookup[song_lookup$Artist=="Lady Gaga" & song_lookup$Track=="Dope","id"] = "1gPCk3KUE83rPdz9QqGsX9"
song_lookup[song_lookup$Artist=="Milkman" & song_lookup$Track=="Summertime (feat. Brandon Skeie)","id"] = "2dGbPVhBJ2I7epNz5NmsVn"
song_lookup[song_lookup$Artist=="Milkman" & song_lookup$Track=="Summertime (feat. Brandon Skeie)","artist_id"] = "30cKuCL2eREKcQ8NqyXUo7"
song_lookup[song_lookup$Artist=="Ava Max" & song_lookup$Track=="Dancing's Done","id"] = "2wqxcctWptasX4VnP2sRvV"
song_lookup[song_lookup$Artist=="Ava Max" & song_lookup$Track=="Million Dollar Baby","id"] = "3wFM8TWbbkwPuB6TyqpVDv"
song_lookup[song_lookup$Artist=="Ava Max" & song_lookup$Track=="Weapons","id"] = "4QXww31EdLcenZiRqnTZeg"
song_lookup[song_lookup$Artist=="Bebe Rexha" & song_lookup$Track=="Heart Wants What It Wants","id"] = "7rS4oy2qpJNULQAVgJQTun"
song_lookup[song_lookup$Artist=="Charli XCX" & song_lookup$Track=="Break the Rules","id"] = "0A5qqtFMG1qrNQEcjvr1A2"
song_lookup[song_lookup$Artist=="John De Sohn" & song_lookup$Track=="I Met Somebody","id"] = "4wFxu6EL2wjhCQ9qs1CeKi"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Christmas Eve","id"] = "3JKvHtrY7PXO7afJ9m6IG0"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Christmas Isn't Canceled (Just You)","id"] = "4zHDuDQx8dcWVmVVtyIzRO"
song_lookup[song_lookup$Artist=="Kelly Clarkson" & song_lookup$Track=="Santa Can't You Hear Me","id"] = "2O3MQ6H3gjrIWDcpeTrikT"
song_lookup[song_lookup$Artist=="Kenny G" & song_lookup$Track=="Auld Lang Syne","id"] = "7h4FywzqhgZrrvpiLDJ2vw"
song_lookup[song_lookup$Artist=="Kenny G" & song_lookup$Track=="Have Yourself a Merry Little Christmas","id"] = "2drJfob9PoGVtQoe08NRvE"
song_lookup[song_lookup$Artist=="Kylie Minogue" & song_lookup$Track=="Get Outta My Way","id"] = "2VZ5Vtjn16RThAvaFz3sJZ"
song_lookup[song_lookup$Artist=="Laidback Luke" & song_lookup$Track=="XOXO (feat. Ina)","id"] = "0AJXiSkJiSuMpnlEeM3Lc7"
song_lookup[song_lookup$Artist=="Molly Sanden" & song_lookup$Track=="Why am I Crying","id"] = "0hNu9mP4Fp5hqiitdrzDKo"
song_lookup[song_lookup$Artist=="Seeb" & song_lookup$Track=="Don't You Wanna Play?","id"] = "5nY4lHwwJcQXV2TKNgaOhU"
song_lookup[song_lookup$Artist=="Shontelle" & song_lookup$Track=="Impossible - Main","id"] = "1T8PwF3JhIiWwBhun23RNe"
song_lookup[song_lookup$Artist=="Stevie Wonder" & song_lookup$Track=="What Christmas Means to Me","id"] = "1V0qqWBbIWt8hlAjxTZedR"  
song_lookup[song_lookup$Artist=="Alan Jackson" & song_lookup$Track=="Where Were You (When the World Stopped Turning)","id"] = "4aOQG9TYcZOhT3sngkMI9K"
song_lookup[song_lookup$Artist=="Celine Dion" & song_lookup$Track=="Don't Save It All for Christmas Day","id"] = "4ruIMJnh6mPhiNWE9amaEF"
song_lookup[song_lookup$Artist=="Celine Dion" & song_lookup$Track=="O Holy Night","id"] = "5547x0ZawobhfSeh2QKAca"
song_lookup[song_lookup$Artist=="Christina Aguilera" & song_lookup$Track=="Have Yourself a Merry Little Christmas","id"] = "0iV9yfj1knFNOSEiuTHZwl"
song_lookup[song_lookup$Artist=="Christina Aguilera" & song_lookup$Track=="This Christmas","id"] = "32h59T8q2SonUPJ006lyXt"
song_lookup[song_lookup$Artist=="Ella Henderson" & song_lookup$Track=="Dream on Me","id"] = "6vHE5zEPYdb0HUKRaIQ6tI"
song_lookup[song_lookup$Artist=="Johnny Mathis" & song_lookup$Track=="Winter Wonderland","id"] = "1JtYy7MFUIZM0MREJaZhTP"
song_lookup[song_lookup$Artist=="Johnny Mathis" & song_lookup$Track=="It's Beginning to Look a Lot Like Christmas","id"] = "4jwZZX8J8eN3w0FOK4FCI3"
song_lookup[song_lookup$Artist=="Katy Perry" & song_lookup$Track=="Never Really Over","id"] = "5PYQUBXc7NYeI1obMKSJK0"
song_lookup[song_lookup$Artist=="Seeb" & song_lookup$Track=="Fade Out","id"] = "0sAHkOX37gvRCEjOJ0WYZ3"
song_lookup[song_lookup$Artist=="The Ronettes" & song_lookup$Track=="I Saw Mommy Kissing Santa Claus","id"] = "4h7zzeg7EUP4B7zGPU9Wx1"
song_lookup[song_lookup$Artist=="Whitney Houston" & song_lookup$Track=="Do You Hear What I Hear?","id"] = "5umJVEAPT2SocCoB99ZoaH"
song_lookup[song_lookup$Artist=="Bebe Rexha" & song_lookup$Track=="Call on Me","id"] = "2GxkC3PeOaUd1FBVHkAyOU"
song_lookup[song_lookup$Artist=="Thomas Helmore" & song_lookup$Track=="A Christmas Festival Medley: Joy To The World / Deck The Halls / Good King Wenceslas / God Rest Ye Merry Gentlemen / Hark! The Herald Angels Sing / The First Noel / Silent Night / Jingle Bells / O Come All Ye Faithful","id"] = "7KMpLYailbVjukHFUJuO1V"
song_lookup[song_lookup$Artist=="STANDERWICK" & song_lookup$Track=="All Of Us - Radio Edit","id"] = "3rCsR7grPZWxQVnuy9dKdK"
song_lookup[song_lookup$Artist=="Antillas" & song_lookup$Track=="The Love - Spark & Shade Radio Edit","id"] = "6eHglVPiByEX5cRRPefO5X"

attributes_lookup = song_lookup

#Check No Duplicates
check = attributes_lookup %>%
  group_by(id) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

check_artist = attributes_lookup %>%
                group_by(Artist,artist_id) %>%
                summarize(thecount = n()) %>%
                group_by(Artist) %>%
                summarize(count = n()) %>%
                arrange(desc(count))

#Lookup Song Attributes
options(scipen=999)

attributes_lookup$danceability = NA
attributes_lookup$energy = NA
attributes_lookup$key = NA
attributes_lookup$loudness = NA
attributes_lookup$mode = NA
attributes_lookup$speechiness = NA
attributes_lookup$acousticness = NA
attributes_lookup$instrumentalness = NA
attributes_lookup$liveness = NA
attributes_lookup$valence = NA
attributes_lookup$tempo = NA
attributes_lookup$duration_ms = NA
attributes_lookup$time_signature = NA
attributes_lookup$explicit = NA
attributes_lookup$song_popularity = NA
attributes_lookup$album_name = NA
attributes_lookup$album_id = NA

for(i in seq(from=1,to=dim(attributes_lookup)[1],by=1)){
  cat(i,"\n",sep="")
  theLookup = attributes_lookup[i,"id"]
  temp = glue('\'curl -X "GET" "https://api.spotify.com/v1/audio-features/{theLookup}" -H "Accept: application/json" -H "Content-Type: application/json" -H "Authorization: Bearer {OAuth_Token}"\'')
  temp2 = glue('\'curl -X "GET" "https://api.spotify.com/v1/tracks/{theLookup}" -H "Accept: application/json" -H "Content-Type: application/json" -H "Authorization: Bearer {OAuth_Token}"\'')
  holder = fromJSON(system(substr(temp,2,nchar(temp)-1),inter=T,ignore.stderr = T))
  holder2 = fromJSON(system(substr(temp2,2,nchar(temp)-1),inter=T,ignore.stderr = T))
  attributes_lookup[i,"danceability"] = holder$danceability
  attributes_lookup[i,"energy"] = holder$energy
  attributes_lookup[i,"key"] = holder$key
  attributes_lookup[i,"loudness"] = holder$loudness
  attributes_lookup[i,"mode"] = holder$mode
  attributes_lookup[i,"speechiness"] = holder$speechiness
  attributes_lookup[i,"acousticness"] = holder$acousticness
  attributes_lookup[i,"instrumentalness"] = holder$instrumentalness
  attributes_lookup[i,"liveness"] = holder$liveness
  attributes_lookup[i,"valence"] = holder$valence
  attributes_lookup[i,"tempo"] = holder$tempo
  attributes_lookup[i,"duration_ms"] = holder$duration_ms
  attributes_lookup[i,"time_signature"] = holder$time_signature
  attributes_lookup[i,"explicit"] = holder2$explicit
  attributes_lookup[i,"song_popularity"] = holder2$popularity
  attributes_lookup[i,"album_name"] = holder2$album$name
  attributes_lookup[i,"album_id"] = holder2$album$id
}

attributes_lookup$key = as.factor(attributes_lookup$key)
attributes_lookup$mode = as.factor(attributes_lookup$mode)
attributes_lookup$time_signature = as.factor(attributes_lookup$time_signature)

#Quick fixes for album mappings
attributes_lookup[attributes_lookup$Artist=="Carly Rae Jepsen" & attributes_lookup$album_name=="Emotion","album_id"] = "2oj3FG6fos7zAQJxLQGzou"
attributes_lookup[attributes_lookup$Artist=="Carly Rae Jepsen" & attributes_lookup$album_name=="Emotion","album_name"] = "Emotion (Deluxe Expanded Edition)"
attributes_lookup[attributes_lookup$Artist=="Kelly Clarkson" & attributes_lookup$album_name=="Wrapped In Red (Deluxe Version)","album_id"] = "7Jahqd1kx9Qau0E9x9iZj6"
attributes_lookup[attributes_lookup$Artist=="Kelly Clarkson" & attributes_lookup$album_name=="Wrapped In Red (Deluxe Version)","album_name"] = "Wrapped In Red"
attributes_lookup[attributes_lookup$Artist=="Lady Gaga" & attributes_lookup$album_name=="A Star Is Born Soundtrack (Without Dialogue)","album_id"] = "4sLtOBOzn4s3GDUv3c5oJD"
attributes_lookup[attributes_lookup$Artist=="Lady Gaga" & attributes_lookup$album_name=="A Star Is Born Soundtrack (Without Dialogue)","album_name"] = "A Star Is Born Soundtrack"
attributes_lookup[attributes_lookup$Artist=="Lady Gaga" & attributes_lookup$album_name=="Born This Way","album_id"] = "5maeycU97NHBgwRr2h2A4O"
attributes_lookup[attributes_lookup$Artist=="Lady Gaga" & attributes_lookup$album_name=="Born This Way","album_name"] = "Born This Way (Special Edition)"
attributes_lookup[attributes_lookup$Artist=="Lady Gaga" & attributes_lookup$album_name=="Sour Candy (with BLACKPINK)","album_id"] = "05c49JgPmL4Uz2ZeqRx5SP"
attributes_lookup[attributes_lookup$Artist=="Lady Gaga" & attributes_lookup$album_name=="Sour Candy (with BLACKPINK)","album_name"] = "Chromatica"
attributes_lookup[attributes_lookup$Artist=="Lady Gaga" & attributes_lookup$album_name=="The Fame","album_id"] = "1jpUMnKpRlng1OJN7LJauV"
attributes_lookup[attributes_lookup$Artist=="Ava Max" & 
                    attributes_lookup$Track %in% c("Million Dollar Baby","Dancing's Done","Maybe You're the Problem","Weapons"),"album_id"] = "5NgQit6EhPnx84bxgVVDRQ"
attributes_lookup[attributes_lookup$Artist=="Ava Max" & 
                    attributes_lookup$Track %in% c("Million Dollar Baby","Dancing's Done","Maybe You're the Problem","Weapons"),"album_name"] = "Diamonds & Dancefloors"
attributes_lookup[attributes_lookup$Artist=="Ava Max" & attributes_lookup$Track=="EveryTime I Cry","album_id"] = "5W79aGcuoBYk0Mb2QL2Jcw"
attributes_lookup[attributes_lookup$Artist=="Ava Max" & attributes_lookup$Track=="EveryTime I Cry","album_name"] = "EveryTime I Cry"
attributes_lookup[attributes_lookup$Artist=="Betty Who" & attributes_lookup$album_name=="Take Me When You Go (Deluxe Version)","album_id"] = "3c2TzUftOakyVsesUMgAOB"
attributes_lookup[attributes_lookup$Artist=="Betty Who" & attributes_lookup$album_name=="Take Me When You Go (Deluxe Version)","album_name"] = "Take Me When You Go"
attributes_lookup[attributes_lookup$Artist=="Charli XCX" & attributes_lookup$album_name=="CRASH","album_id"] = "3lb7EyEcWhZOK0SpZ2dNpn"
attributes_lookup[attributes_lookup$Artist=="Charli XCX" & attributes_lookup$album_name=="CRASH","album_name"] = "CRASH (Deluxe)"
attributes_lookup[attributes_lookup$Artist=="Dua Lipa" & attributes_lookup$album_name=="Future Nostalgia","album_id"] = "0JeyP8r2hBxYIoxXv11XiX"
attributes_lookup[attributes_lookup$Artist=="Dua Lipa" & attributes_lookup$album_name=="Future Nostalgia","album_name"] = "Future Nostalgia (The Moonlight Edition)"
attributes_lookup[attributes_lookup$Artist=="Ellie Goulding" & str_detect(attributes_lookup$album_name,"Halcyon"),"album_id"] = "1gKVOJVxDTqw9IaCbKaYd4"
attributes_lookup[attributes_lookup$Artist=="Ellie Goulding" & str_detect(attributes_lookup$album_name,"Halcyon"),"album_name"] = "Halcyon Nights"
attributes_lookup[attributes_lookup$Artist=="Ellie Goulding" & 
                    attributes_lookup$Track %in% c("Lights","Starry Eyed","Your Song"),"album_id"] = "4oVG376KpWeBaxrKqRxVri"
attributes_lookup[attributes_lookup$Artist=="Ellie Goulding" & 
                    attributes_lookup$Track %in% c("Lights","Starry Eyed","Your Song"),"album_name"] = "Lights"
attributes_lookup[attributes_lookup$Artist=="Emeli Sande" & attributes_lookup$album_name=="Our Version Of Events","album_id"] = "6vAgHrX0THw3dFEpk1Cjt3"
attributes_lookup[attributes_lookup$Artist=="Emeli Sande" & attributes_lookup$album_name=="Our Version Of Events","album_name"] = "Our Version Of Events (Special Edition)"
attributes_lookup[attributes_lookup$Artist=="Gryffin" & attributes_lookup$album_name=="Baggage (with AlunaGeorge)","album_id"] = "2IAVHJdaRPFA6MQqXHoG75"
attributes_lookup[attributes_lookup$Artist=="Gryffin" & attributes_lookup$album_name=="Baggage (with AlunaGeorge)","album_name"] = "Gravity"
attributes_lookup[attributes_lookup$Artist=="Gryffin" & attributes_lookup$album_name=="OMG (with Carly Rae Jepsen)","album_id"] = "2IAVHJdaRPFA6MQqXHoG75"
attributes_lookup[attributes_lookup$Artist=="Gryffin" & attributes_lookup$album_name=="OMG (with Carly Rae Jepsen)","album_name"] = "Gravity"
attributes_lookup[attributes_lookup$Artist=="Katy Perry" & attributes_lookup$album_name=="PRISM","album_id"] = "5MQBzs5YlZlE28mD9yUItn"
attributes_lookup[attributes_lookup$Artist=="Katy Perry" & attributes_lookup$album_name=="PRISM","album_name"] = "PRISM (Deluxe)"
attributes_lookup[attributes_lookup$Artist=="Katy Perry" & attributes_lookup$album_name=="Teenage Dream","album_id"] = "5BvgP623rtvlc0HDcpzquz"
attributes_lookup[attributes_lookup$Artist=="Katy Perry" & attributes_lookup$album_name=="Teenage Dream","album_name"] = "Teenage Dream: The Complete Confection"
attributes_lookup[attributes_lookup$Artist=="Kim Petras" & attributes_lookup$album_name=="TURN OFF THE LIGHT, VOL. 1","album_id"] = "6uqXwF2cBNS3V4fw8YM575"
attributes_lookup[attributes_lookup$Artist=="Kim Petras" & attributes_lookup$album_name=="TURN OFF THE LIGHT, VOL. 1","album_name"] = "TURN OFF THE LIGHT"
attributes_lookup[attributes_lookup$Artist=="Nicki Minaj" & attributes_lookup$album_name=="Pink Friday ... Roman Reloaded","album_id"] = "6fABwONLawdFjkDpLx41j8"
attributes_lookup[attributes_lookup$Artist=="Nicki Minaj" & attributes_lookup$album_name=="Pink Friday ... Roman Reloaded","album_name"] = "Pink Friday ... Roman Reloaded (Deluxe)"
attributes_lookup[attributes_lookup$Artist=="Rihanna" & attributes_lookup$album_name=="Good Girl Gone Bad","album_id"] = "3JSWZWeTHF4HDGt5Eozdy7"
attributes_lookup[attributes_lookup$Artist=="Rihanna" & attributes_lookup$album_name=="Good Girl Gone Bad","album_name"] = "Good Girl Gone Bad: Reloaded"
attributes_lookup[attributes_lookup$Artist=="Rihanna" & attributes_lookup$album_name=="Loud (Japan Version)","album_id"] = "6UHhmTLl9T1scRYLmpHcDX"
attributes_lookup[attributes_lookup$Artist=="Rihanna" & attributes_lookup$album_name=="Loud (Japan Version)","album_name"] = "Loud"
attributes_lookup[attributes_lookup$Artist=="Seeb" & 
                    attributes_lookup$Track %in% c("Free To Go","Don't You Wanna Play?","Fade Out","Drink About"),"album_id"] = "116cozWS2DlOUFwYDSCovA"
attributes_lookup[attributes_lookup$Artist=="Seeb" & 
                    attributes_lookup$Track %in% c("Free To Go","Don't You Wanna Play?","Fade Out","Drink About"),"album_name"] = "Sad in Scandinavia"
attributes_lookup[attributes_lookup$Artist=="Taylor Swift" & attributes_lookup$album_name=="1989","album_id"] = "34OkZVpuzBa9y40DCy0LPR"
attributes_lookup[attributes_lookup$Artist=="Taylor Swift" & attributes_lookup$album_name=="1989","album_name"] = "1989 (Deluxe Edition)"


#Get artist features
artist_lookup = song_lookup %>% group_by(Artist,artist_id) %>% summarise(count = sum(streams))

artist_lookup$genres = NA
artist_lookup$followers = NA
artist_lookup$artist_popularity = NA

for(i in seq(from=1,to=dim(artist_lookup)[1],by=1)){
  cat(i,"\n",sep="")
  theLookup = artist_lookup[i,"artist_id"]
  temp = glue('\'curl -X "GET" "https://api.spotify.com/v1/artists/{theLookup}" -H "Accept: application/json" -H "Content-Type: application/json" -H "Authorization: Bearer {OAuth_Token}"\'')
  holder = fromJSON(system(substr(temp,2,nchar(temp)-1),inter=T,ignore.stderr = T))
  artist_lookup[i,"genres"] = paste(holder$genres,collapse=",")
  artist_lookup[i,"followers"] = holder$followers$total
  artist_lookup[i,"artist_popularity"] = holder$popularity
}

#Limit genre features only to the most popular
genres = as.data.frame(sort(unique(unlist(str_split(dplyr::pull(artist_lookup,genres),",")))))
colnames(genres)[1] = "genre"
genres = genres %>% filter(genre != "")
genres$count = NA
genres$artists = NA

for(i in seq(from=1,to=dim(genres)[1],by=1)){
  cat(i,"\n",sep="")
  str1 = paste(trimws(genres[i,"genre"]),",",sep="")
  str2 = paste(",",trimws(genres[i,"genre"]),sep="")
  str3 = trimws(genres[i,"genre"])
  holder = artist_lookup %>% filter(str_detect(genres,str1) |
                                    str_detect(genres,str2) |
                                    genres == str3)
  genres[i,"count"] = sum(holder[,"count"]) 
  genres[i,"artists"] = dim(holder)[1]
}

top_genres = genres %>% filter(count > sum(artist_lookup[,"count"])*.025)


for(j in seq(from=1,to=dim(top_genres)[1],by=1)){
  str1 = paste(trimws(top_genres[j,"genre"]),",",sep="")
  str2 = paste(",",trimws(top_genres[j,"genre"]),sep="")
  str3 = trimws(top_genres[j,"genre"])
  artist_lookup = artist_lookup %>% mutate(v = ifelse(str_detect(genres,str1) | str_detect(genres,str2) |genres == str3,1,0))
  colnames(artist_lookup)[dim(artist_lookup)[2]] = str3
}
artist_lookup = artist_lookup %>% select(-c(count))

#Cluster Songs
attrs = attributes_lookup
norm = cbind(as.data.frame(attrs[,c(1:6,9,11)]),as.data.frame(scale(attrs[,c(7,8,10,12:17)])))
norm$mode = as.numeric(as.character(norm$mode))

plot(norm[,c(8:17)],pch=20)

pcs = prcomp(norm[,c(8:17)])
plot(pcs)
plot(pcs, type='l')
summary(pcs)

#Look at diminishing marginal returns of PCs
plot_pcs = as.data.frame(pcs$sdev) %>%
            rename(stdev = `pcs$sdev`) %>%
            mutate(PC = row_number(),
                   var = stdev^2)
plot_pcs$prop_of_var = plot_pcs$var / sum(plot_pcs$var)
plot_pcs$cuml_prop_of_var = NA
plot_pcs$cuml_var = NA
for(i in seq(from=1,to=dim(plot_pcs)[1],by=1)){
  if(i==1){
    plot_pcs[i,"cuml_prop_of_var"] = plot_pcs[i,"prop_of_var"]
    plot_pcs[i,"cuml_var"] = plot_pcs[i,"var"]
  } else {
    plot_pcs[i,"cuml_prop_of_var"] = plot_pcs[i-1,"cuml_prop_of_var"] + plot_pcs[i,"prop_of_var"]
    plot_pcs[i,"cuml_var"] = plot_pcs[i-1,"cuml_var"] + plot_pcs[i,"var"]
  }
}
plot_pcs$var_per_comp = plot_pcs$cuml_var / plot_pcs$PC
plot_pcs$prop_of_var_per_comp = plot_pcs$cuml_prop_of_var / plot_pcs$PC

plot(plot_pcs$cuml_prop_of_var,type="l")
plot(plot_pcs$var_per_comp,type="l")

#Low dimensionality... so keeping all PCs
#Plot Principal Components
comp = data.frame(pcs$x[,1:10])
plot(comp, pch=16)

set.seed(412)
km = matrix(nrow=40,ncol=3)
for(i in seq(1:40)){
  kmeans = kmeans(comp,centers = i,iter.max = 1000,nstart=25)
  km[i,1] = i
  km[i,2] = kmeans$tot.withinss
  km[i,3] = kmeans$tot.withinss / kmeans$totss
}
km = as.data.frame(km)
colnames(km) = c("Clusters","TotWithinSS","Prct_TSS")
plot(km$Clusters,km$Prct_TSS,type = "b")

#choose 8... really starts to flatten out after that point
kmeans = kmeans(comp,centers = 8,iter.max = 1000,nstart=25)
norm$cluster = kmeans$cluster


#Cluster Plots
palette(c("blue","green","skyblue","purple","orange","red","maroon1","gold"))
plot(norm[,c(8:17)],col=norm$cluster,pch=16)

#Less Busy - Only select songs with 50+ plays
norm_subset = norm %>% filter(streams >= 50)
plot(norm_subset[,c(8:17)],col=norm$cluster,pch=16)

#Re-Attach Clusters to Non-Normalized Dataset...Write out file
attrs$cluster = as.character(norm$cluster)

for(i in seq(from=1,to=dim(attrs)[1],by=1)){
  if (attrs[i,"cluster"] == 1) { attrs[i,"cluster"] = '2. "The Feels"'}
  else if (attrs[i,"cluster"] == 2) { attrs[i,"cluster"] = '6: Energy Pop'}
  else if (attrs[i,"cluster"] == 3) { attrs[i,"cluster"] = "5: Live Songs"}
  else if (attrs[i,"cluster"] == 4) { attrs[i,"cluster"] = '3: "Speak" Pop'}
  else if (attrs[i,"cluster"] == 5) { attrs[i,"cluster"] = "1: Instrumental Songs"}
  else if (attrs[i,"cluster"] == 6) { attrs[i,"cluster"] = "4: Sad Bops"}
  else if (attrs[i,"cluster"] == 7) { attrs[i,"cluster"] = "7: Dance Songs"}
  else if (attrs[i,"cluster"] == 8) { attrs[i,"cluster"] = "8: Anthem Songs"}
}
attrs$cluster = as.factor(attrs$cluster)
write.csv(attrs,"song_data.csv",row.names = F)

#Density Plots by Cluster
png(filename = "Plots/Summary/Danceability.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=danceability,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Danceability") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Energy.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=energy,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Energy") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Loudness.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=loudness,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Loudness") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Speechiness.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=speechiness,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Speechiness") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Acousticness.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=acousticness,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Acousticness") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Instrumentalness.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=instrumentalness,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Instrumentalness") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Liveness.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=liveness,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Liveness") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Valence.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=valence,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Valence") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Plots/Summary/Tempo.png",width = 1920,height = 1080)
ggplot(attrs, aes(x=cluster, y=tempo,col=cluster)) + 
  geom_boxplot() + 
  scale_color_manual(values=c("blue","green","skyblue","purple","orange","red","maroon1","gold")) +
  ggtitle("Tempo") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

#Re-Attach data to qsongs dataset and create time variables
song_final = qsongs %>% left_join(.,attributes_lookup %>% select(Artist,Track,id,artist_id))

dtimes = song_final[,"Timestamp"]
dtparts = t(as.data.frame(strsplit(dtimes,' ')))
row.names(dtparts) = NULL

song_final$day = dtparts[,1]
song_final$month = dtparts[,2]
song_final$year = dtparts[,3]
song_final$time = dtparts[,4]
song_final$hour = as.numeric(substr(song_final$time,1,2))
song_final$monthyear = paste(song_final$month,song_final$year,sep=" ")

song_final$quarters = NA
song_final[song_final$monthyear=="Oct 2015" |
             song_final$monthyear=="Nov 2015" |
             song_final$monthyear=="Dec 2015","quarters"] = "2015Q4"
song_final[song_final$monthyear=="Jan 2016" |
             song_final$monthyear=="Feb 2016" |
             song_final$monthyear=="Mar 2016","quarters"] = "2016Q1"
song_final[song_final$monthyear=="Apr 2016" |
             song_final$monthyear=="May 2016" |
             song_final$monthyear=="Jun 2016","quarters"] = "2016Q2"
song_final[song_final$monthyear=="Jul 2016" |
             song_final$monthyear=="Aug 2016" |
             song_final$monthyear=="Sep 2016","quarters"] = "2016Q3"
song_final[song_final$monthyear=="Oct 2016" |
             song_final$monthyear=="Nov 2016" |
             song_final$monthyear=="Dec 2016","quarters"] = "2016Q4"
song_final[song_final$monthyear=="Jan 2016" |
             song_final$monthyear=="Feb 2016" |
             song_final$monthyear=="Mar 2017","quarters"] = "2017Q1"
song_final[song_final$monthyear=="Apr 2017" |
             song_final$monthyear=="May 2017" |
             song_final$monthyear=="Jun 2017","quarters"] = "2017Q2"
song_final[song_final$monthyear=="Jul 2017" |
             song_final$monthyear=="Aug 2017" |
             song_final$monthyear=="Sep 2017","quarters"] = "2017Q3"
song_final[song_final$monthyear=="Oct 2017" |
             song_final$monthyear=="Nov 2017" |
             song_final$monthyear=="Dec 2017","quarters"] = "2017Q4"
song_final[song_final$monthyear=="Jan 2018" |
             song_final$monthyear=="Feb 2018" |
             song_final$monthyear=="Mar 2018","quarters"] = "2018Q1"
song_final[song_final$monthyear=="Apr 2018" |
             song_final$monthyear=="May 2018" |
             song_final$monthyear=="Jun 2018","quarters"] = "2018Q2"
song_final[song_final$monthyear=="Jul 2018" |
             song_final$monthyear=="Aug 2018" |
             song_final$monthyear=="Sep 2018","quarters"] = "2018Q3"
song_final[song_final$monthyear=="Oct 2018" |
             song_final$monthyear=="Nov 2018" |
             song_final$monthyear=="Dec 2018","quarters"] = "2018Q4"
song_final[song_final$monthyear=="Jan 2019" |
             song_final$monthyear=="Feb 2019" |
             song_final$monthyear=="Mar 2019","quarters"] = "2019Q1"
song_final[song_final$monthyear=="Apr 2019" |
             song_final$monthyear=="May 2019" |
             song_final$monthyear=="Jun 2019","quarters"] = "2019Q2"
song_final[song_final$monthyear=="Jul 2019" |
             song_final$monthyear=="Aug 2019" |
             song_final$monthyear=="Sep 2019","quarters"] = "2019Q3"
song_final[song_final$monthyear=="Oct 2019" |
             song_final$monthyear=="Nov 2019" |
             song_final$monthyear=="Dec 2019","quarters"] = "2019Q4"
song_final[song_final$monthyear=="Jan 2020" |
             song_final$monthyear=="Feb 2020" |
             song_final$monthyear=="Mar 2020","quarters"] = "2020Q1"
song_final[song_final$monthyear=="Apr 2020" |
             song_final$monthyear=="May 2020" |
             song_final$monthyear=="Jun 2020","quarters"] = "2020Q2"
song_final[song_final$monthyear=="Jul 2020" |
             song_final$monthyear=="Aug 2020" |
             song_final$monthyear=="Sep 2020","quarters"] = "2020Q3"
song_final[song_final$monthyear=="Oct 2020" |
             song_final$monthyear=="Nov 2020" |
             song_final$monthyear=="Dec 2020","quarters"] = "2020Q4"
song_final[song_final$monthyear=="Jan 2021" |
             song_final$monthyear=="Feb 2021" |
             song_final$monthyear=="Mar 2021","quarters"] = "2021Q1"
song_final[song_final$monthyear=="Apr 2021" |
             song_final$monthyear=="May 2021" |
             song_final$monthyear=="Jun 2021","quarters"] = "2021Q2"
song_final[song_final$monthyear=="Jul 2021" |
             song_final$monthyear=="Aug 2021" |
             song_final$monthyear=="Sep 2021","quarters"] = "2021Q3"
song_final[song_final$monthyear=="Oct 2021" |
             song_final$monthyear=="Nov 2021" |
             song_final$monthyear=="Dec 2021","quarters"] = "2021Q4"
song_final[song_final$monthyear=="Jan 2022" |
             song_final$monthyear=="Feb 2022" |
             song_final$monthyear=="Mar 2022","quarters"] = "2022Q1"
song_final[song_final$monthyear=="Apr 2022" |
             song_final$monthyear=="May 2022" |
             song_final$monthyear=="Jun 2022","quarters"] = "2022Q2"
song_final[song_final$monthyear=="Jul 2022" |
             song_final$monthyear=="Aug 2022" |
             song_final$monthyear=="Sep 2022","quarters"] = "2022Q3"
song_final[song_final$monthyear=="Oct 2022" |
             song_final$monthyear=="Nov 2022" |
             song_final$monthyear=="Dec 2022","quarters"] = "2022Q4"
song_final[song_final$monthyear=="Jan 2023" |
             song_final$monthyear=="Feb 2023" |
             song_final$monthyear=="Mar 2023","quarters"] = "2023Q1"
song_final[song_final$monthyear=="Apr 2023" |
             song_final$monthyear=="May 2023" |
             song_final$monthyear=="Jun 2023","quarters"] = "2023Q2"
song_final[song_final$monthyear=="Jul 2023" |
             song_final$monthyear=="Aug 2023" |
             song_final$monthyear=="Sep 2023","quarters"] = "2023Q3"
song_final[song_final$monthyear=="Oct 2023" |
             song_final$monthyear=="Nov 2023" |
             song_final$monthyear=="Dec 2023","quarters"] = "2023Q4"
song_final$monthyear = as.yearmon(paste(song_final$month,substr(song_final$year,3,4),sep="-"),"%b-%y")
song_final$count = 1

write.csv(song_final,"stream_data.csv",row.names = F)

#Write out the artist information
write.csv(artist_lookup,"artist_data.csv",row.names=F)

