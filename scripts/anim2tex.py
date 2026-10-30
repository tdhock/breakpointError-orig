def process_commands(text):
    "Scan text for animation commands and process any found."
    chunks=text.split("ANIMATION(")
    results=chunks[0]
    for chunk in chunks[1:]:
        pieces=chunk.split(")")
        f=pieces[0]+".Rnw"
        result=process_template(f)
        results += result+")".join(pieces[1:])
    return results
def process_template(f):
    "Scan template file for %s and fill in."
    lines=open(f).readlines()
    items=lines[0].replace("%","").split()
    print "%s: %s"%(f, ", ".join(items))
    text="".join(lines[1:])
    slides=[text.replace("%s",i) for i in items]
    return '\n'.join(slides)
if __name__ == "__main__":
    import sys
    if len(sys.argv)!=3:
        print "Usage: %s file.Ranim file.Rnw"%sys.argv[0]
        sys.exit(1)
    cmd, anim_filename, sweave_filename = sys.argv
    commands = open(anim_filename).read()
    f=open(sweave_filename,"w")
    f.write(process_commands(commands))
    f.close()
