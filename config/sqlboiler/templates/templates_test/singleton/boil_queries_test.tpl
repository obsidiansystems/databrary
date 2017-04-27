var dbNameRand *rand.Rand

func MustTx(transactor boil.Transactor, err error) boil.Transactor {
	if err != nil {
		panic(fmt.Sprintf("Cannot create a transactor: %s", err))
	}
	return transactor
}

var (
	rgxPGFkey   = regexp.MustCompile(`(?m)^ALTER TABLE ONLY .*\n\s+ADD CONSTRAINT .*? FOREIGN KEY .*?;\n`)
	rgxPGPkey   = regexp.MustCompile(`(?m)^ALTER TABLE ONLY .*\n\s+ADD CONSTRAINT .*? PRIMARY KEY .*?;\n`)
	rgxPGCon    = regexp.MustCompile(`(?m)^ALTER TABLE ONLY .*\n\s+ADD CONSTRAINT .*?;\n`)
	rgxPGConEx  = regexp.MustCompile(`(?m)^ALTER TABLE ONLY .*\n\s+ADD CONSTRAINT .*? EXCLUDE .*?;\n`)
	rgxPGTrig   = regexp.MustCompile(`(?m)^CREATE TRIGGER .*?;\n`)
	rgxPGDefer  = regexp.MustCompile(`DEFERRABLE`)
	rgxMySQLkey = regexp.MustCompile(`(?m)((,\n)?\s+CONSTRAINT.*?FOREIGN KEY.*?\n)+`)
)

func newFKeyDestroyer(regex *regexp.Regexp, reader io.Reader) io.Reader {
	return &fKeyDestroyer{
		reader: reader,
		rgx:    regex,
	}
}

type fKeyDestroyer struct {
	reader io.Reader
	buf    *bytes.Buffer
	rgx    *regexp.Regexp
}

func (f *fKeyDestroyer) Read(b []byte) (int, error) {
	if f.buf == nil {
		all, err := ioutil.ReadAll(f.reader)
		if err != nil {
			return 0, err
		}
		newSchema := f.rgx.ReplaceAll(all, []byte{})
		f.buf = bytes.NewBuffer(newSchema)
	}
	return f.buf.Read(b)
}

var rgxCheckCon = regexp.MustCompile(`(?m)CHECK \((.*)\).*?\n`)

func newConReplacer(regex *regexp.Regexp, reader io.Reader) io.Reader {
	return &conDestroyer{
		reader: reader,
		rgx:    regex,
	}
}

type conDestroyer struct {
	reader io.Reader
	buf    *bytes.Buffer
	rgx    *regexp.Regexp
}

func (f *conDestroyer) Read(b []byte) (int, error) {
	if f.buf == nil {
		all, err := ioutil.ReadAll(f.reader)
		allString := string(all)
		allRune := []rune(allString)
		if err != nil {
			return 0, err
		}
		newSchema := f.rgx.FindAllStringSubmatchIndex(allString, -1)
		for i := len(newSchema) - 1; i >= 0; i-- {
			v := newSchema[i]
			_, _, cs, ce := v[0], v[1], v[2], v[3]
			allbeg := append(allRune[0:cs], []rune("true")...)
			allRune = append(allbeg, allRune[ce:]...)
		}
		f.buf = bytes.NewBuffer([]byte(string(allRune)))
	}
	return f.buf.Read(b)
}
