## Contributing

1. Fork the project
2. Create feature branch: `git checkout -b my-new-feature`
3. Make changes and test
4. Commit: `git commit -am 'add: feature description'`
5. Push: `git push origin my-new-feature`
6. Open pull request

## Build

### Prerequisites
Download/clone these dependencies to folders next to DripGrepper:
- [spring4d](https://bitbucket.org/sglienke/spring4d)
- [FastMM4](https://github.com/pleriche/FastMM4)

Install via Delphi's GetIt Package Manager:
- VirtualTree for VCL
- SVGIconImageList VCL and FMX

Optional: [TestInsight](https://files.spring4d.com/TestInsight/latest/TestInsightSetup.zip)

#### Install GUI Components 
If opening the project in the IDE shows an error such as `Class TIconLabel not found`, Delphi usually cannot resolve the design component class while loading a `.dfm` file.

<img width="517" height="198" alt="ComponentNotFoundMsg" src="https://github.com/user-attachments/assets/aa1b3b7f-83d4-44e2-808b-6a3587576768" />

* In that dialog, press **Cancel**.
* Make sure the missing component unit is included in the `DRipGrepperComponents` project. For newly added components this is required before the package can be installed. 
* ⚠️ For a newly added component class, add a `Register` procedure in the component unit, for example:
```pascal
interface
...
procedure Register;

implementation

procedure Register;
begin
	RegisterComponents(SECTION_NAME, [TIconLabel]);
end;
...
```

* If `DRipGrepperComponents` is already installed, remove it first in the IDE via **Components -> Install Packages...**.
* Now install the `DRipGrepperComponents` package in the IDE. Right click on project, in the context menu choose `Install...`
* After installation, Delphi shows a message box with the list of component classes that were installed.

<img width="915" height="238" alt="InstalledSuccesfull" src="https://github.com/user-attachments/assets/647400b9-0d42-43bd-9968-2706d6036eeb" />
 
* Close and reopen the IDE, then reopen the project. The missing-class error should be gone.
#### Troubleshooting
* If installation doesn't work, (eg. Access violation at address xxxx in module 'rtl280.bpl') try to change the project's resource compiler from Borland (BRCC32) to the Windows SDK, and restart the IDE. (https://en.delphipraxis.net/topic/7775-delphi-112-cannot-create-and-install-new-component/)

* https://docwiki.embarcadero.com/RADStudio/Athens/en/Install_Component
