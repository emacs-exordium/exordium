/** testclasses.hh --- A test file full of classes to make diagrams from.
 */

class MyBaseclass 
{     

public:
  MyBaseclass()
  {  }
  ~MyBaseclass()
  {  }

  /**
   * fMyPrivateData Accessors
   * @{
   */
  int getMyPrivateData() const {
    return fMyPrivateData;
  }
  void setMyPrivateData(int MyPrivateData) {
    fMyPrivateData = MyPrivateData;
  }
  /**
   * @}
   */

private:
  int fMyPrivateData;
  char fMyChar;
};

class Subclass : public MyBaseclass
{     

public:
  Subclass()
  {  }
  ~Subclass()
  {  }

  /**
   * scData Accessors
   * @{
   */
  int getcData() const {
    return scData;
  }
  void setcData(int cData) {
    scData = cData;
  }
  /**
   * @}
   */

private:
  int scData;
};

class SpecificClass : public Subclass
{     

public:
  SpecificClass()
  {  }
  ~SpecificClass()
  {  }

  int addSubData(Subclass *sd) {
    SubData.push(sd);
  }

private:
  int SData;
  vector<Subclass> *SubData;

};

class OtherClass : public Subclass
{     

public:
  OtherClass()
  {  }
  ~OtherClass()
  {  }
  /**
   * fOdata Accessors
   * @{
   */
  int getOdata() const {
    return fOdata;
  }
  void setOdata(int Odata) {
    fOdata = Odata;
  }
  /**
   * @}
   */

private:
  int fOdata;

};

class AltClass : public Subclass
{     

public:
  AltClass()
  {  }
  AltClass()
  {  }
private:
  int fAdata;
};

// End
