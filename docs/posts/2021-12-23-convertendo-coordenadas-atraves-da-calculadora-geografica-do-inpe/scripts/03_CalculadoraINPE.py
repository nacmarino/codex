from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import re

# criando a classe do scrapper
class CalculadoraINPE:
  """ Interacts with INPE's geographic calculator to convert coordinates from one projection and datum to another. """
  
  ## to initiatlize the class
  def __init__(self, driver_path):
    """
    
    Initiatizes the class.
    
    Parameters
    ----------
    driver_path: str
      the local path to the driver executable
    """
    # local path to the driver executable
    self._path_to_driver = driver_path 
    # target url
    self._url = 'http://www.dpi.inpe.br/calcula/' 
    # dictionary to encode the projection into the code used by the site
    self._projections = {
      'GMS': 'latlong_gms', 'GD': 'latlong_gd', 'UTM': 'utm', 'POLICONICA': 'policonica', 
      'LAMBERT': 'lambert', 'MERCATOR': 'mercator', 'ALBERS': 'albers', 
      'CILIN. EQUIDISTANTE': 'cilequi', 'MILLER': 'miller', 'GAUSS': 'gauss'
    } 
    # dictionary to encode the datum into the code used by the site
    self._datums = {'SAD 69': '1', 'CorregoAlegre': '2', 'Astro-Chua': '3', 'WGS84': '4', 'SIRGAS2000': '5'}
    # dictionary to encode the UTM zone 
    self._utm_zone = dict(zip(list(range(11, 31)), ['-0' + str(x * -1) if x > -10 and x < 0 else str(x) for x in list(range(-117, 3, 6))]))
    # dictionary to encode the available output projections
    self._projections_out = {'UTM': 'utm', 'GEO': 'latlong', 'POLICONICA': 'policonica', 
      'LAMBERT': 'lambert', 'MERCATOR': 'mercator', 'ALBERS': 'albers', 
      'CILIN. EQUIDISTANTE': 'cilequi', 'MILLER': 'miller', 'GAUSS': 'gauss'}
    # object to hold the driver
    self._driver = None
  
  ## to open a driver session
  def open_driver(self):
    """
    Opens the web driver executable and initializes a session.
    """
    # setting the options so that we run the driver in headless mode
    options = Options()
    options.headless = True
    
    # instantiating the driver and overwritting the self._driver object
    try:
      self._driver = webdriver.Chrome(executable_path = self._path_to_driver, options = options)
      # priting a status message
      if self._driver.session_id is not None:
        print('Web driver session successfully initialized!')
      else:
        raise Exception('There was an unknown error when initializing the web driver sessions.')
    # throw an exception in case the file is missing
    except FileNotFoundError:
      print(f'File not found at path {self._path_to_driver}.')
  
  ## to enter the page
  def _enter_page(self):
    """
    Open INPE's geographical calculator page.
    """
    if self._driver.session_id is None:
      raise Exception('You need to open a driver session before converting coordinates!')
    elif self._driver.current_url != self._url:
      print("Entering INPE's geographical calculator page.")
      self._driver.get(url = self._url)
      self._driver.implicitly_wait(10)
    elif self._driver.current_url == self._url:
      print("Refreshing INPE's geographical calculator page.")
      self._driver.refresh()
    else:
      raise Exception('Unknown error.')
  
  # function to fill in the first part of the form
  def _fill_first_form(self, proj_in, datum_in, longitude, latitude):
    """
    
    Fill in the first part of the form from INPE's geographical calculator, which represents 
    the input information that should be converted into another projection and/or datum.
    
    Parameters
    ----------
    proj_in: str
      input projection
    datum_in: str
      input datum
    longitude: float or list
      input longitude. If input longitude is in the form of degree-minute-second, then a list with four elements should
      be provided, containing the elements in the exact sequence: a string encoding whether coordinates are towards the 
      west ('Oeste') or east ('Leste') of Greenwhich, followed by the degrees, minutes and seconds.
    latitude: float or list
      input latitude. If input longitude is in the form of degree-minute-second, then a list with four elements should
      be provided, containing the elements in the exact sequence: a string encoding whether coordinates are towards the 
      northern ('Norte') or southern ('South') hemisphere, followed by the degrees, minutes and seconds.
    """
    # select the target iframe
    self._driver.switch_to.frame(frame_reference = 'contents')
    # select the input projection
    select_input_proj = Select(self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[3]//select'))
    select_input_proj.select_by_value(value = self._projections[proj_in])
    # select the input datum
    select_input_datum = Select(self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[9]//select'))
    select_input_datum.select_by_value(value = self._datums[datum_in])
    # pass in the coordinates
    if proj_in in ['GD', 'UTM']:
      # pass in the longitude and latitude, respectively
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[5]//td//input').send_keys(longitude)
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[7]//td//input').send_keys(latitude)
    elif proj_in == 'GMS':
      # longitude data
      Select(self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[5]//select')).select_by_visible_text(text = longitude[0])
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[5]//td//input[@name="xGrau"]').send_keys(longitude[1])
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[5]//td//input[@name="xMin"]').send_keys(longitude[2])
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[5]//td//input[@name="xSeg"]').send_keys(longitude[3])
      # latitude data
      Select(self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[7]//select')).select_by_visible_text(text = latitude[0])
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[7]//td//input[@name="xGrau"]').send_keys(latitude[1])
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[7]//td//input[@name="xMin"]').send_keys(latitude[2])
      self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[7]//td//input[@name="xSeg"]').send_keys(latitude[3])
    else:
      raise NotImplementedError('There is no implementation for input coordinates other than GD, GMS or UTM.')
    # clicking in the button
    self._driver.find_element(by = 'xpath', value='//body//div//center//table//tbody//tr[10]//td').click()
  
  # function to fill in the second part of the form
  def _fill_second_form(self, proj_in, proj_out, datum_out, utm_zone_out, hemisphere):
    """
    Fill in the second part of the form from INPE's geographical calculator, which represents 
    the shape of the output information that it expected.
    
    Parameters
    ----------
    proj_in: str
      input projection
    proj_out: str
      output projection
    datum_out: str
      output datum
    utm_zone_out: str
      target UTM zone: a number from 11 to 30, encoding the UTM zone for the data, if this was the input projection
    hemisphere: str
      target hemisphere: either 'Norte' or 'Sul'
    """
    # passing to the second frame
    self._driver.switch_to.parent_frame()
    self._driver.switch_to.frame(frame_reference = self._driver.find_element_by_name('mainp'))
    # entering the desired outputs if the input projection is not UTM
    if proj_in != 'UTM':
      # selecting the output projection
      select_output_proj = Select(self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[2]//select'))
      select_output_proj.select_by_value(value = self._projections_out[proj_out])
      # selecting the output datum
      select_output_datum = Select(self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[4]//td//select'))
      select_output_datum.select_by_value(value = self._datums[datum_out])
      # clicking in the button
      self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[5]//td').click()
    # entering the desired outputs if the projection is UTM
    else:
      # selecting the target utm zone
      select_utm_zone = Select(self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[3]//td//select'))
      select_utm_zone.select_by_value(value = self._utm_zone[utm_zone_out])
      # selecting the target hemisphere
      select_hemisphere = Select(self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[5]//td//select'))
      select_hemisphere.select_by_visible_text(text = hemisphere)
      # selecting the output projection
      select_output_proj = Select(self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[9]//select'))
      select_output_proj.select_by_value(value = self._projections_out[proj_out])
      # selecting the output datum
      select_output_datum = Select(self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[11]//td//select'))
      select_output_datum.select_by_value(value = self._datums[datum_out])
      # clicking in the button
      self._driver.find_element(by = 'xpath', value='//html//body//table//tbody//tr[12]//td').click()
  
  ## function to get the contents of the last frame
  def _get_last_frame(self):
    """
    Retrieves the raw contents of the frame containing the converted coordinates
    """
    # switching to the new frame and scrapping the contents of the table
    self._driver.switch_to.parent_frame()
    self._driver.switch_to.frame(frame_reference = self._driver.find_element_by_name('canvas'))
    return [x for x in self._driver.find_elements(by = 'xpath', value='//html//body//table//tbody//tr')]
  
  ## function to parse the contents of the output frame
  def _parse_contents(self):
    """ 
    Parse the contents of the frame containing the outputs.
    """
    outputs = {}
    all_rows = self._get_last_frame()
    for x in all_rows:
      # parsing the HTML code
      results = BeautifulSoup(markup = x.get_attribute('innerHTML'), features = 'html.parser').text.strip()
      # splitting the string
      results = re.split('\s{2,}|///', results)
      # saving the output only if it is not one of the two below
      if len(results) > 1 and results[0] != '--':
        outputs[results[0]] = results[1]
      else:
        pass
    
    return outputs
      
  ## function to wrap up all the other functions that convert and extract the coordinates
  def convert_coordinates(self, proj_in, datum_in, longitude, latitude, proj_out, datum_out, utm_zone_out = None, hemisphere = None):
    """ 
    Convert a set of coordinates from one projection and datum into another.
    
    Parameters
    ----------
    proj_in: str
      input projection
    datum_in: str
      input datum
    longitude: float or list
      input longitude. If input longitude is in the form of degree-minute-second, then a list with four elements should
      be provided, containing the elements in the exact sequence: a string encoding whether coordinates are towards the 
      west ('Oeste') or east ('Leste') of Greenwhich, followed by the degrees, minutes and seconds.
    latitude: float or list
      input latitude. If input longitude is in the form of degree-minute-second, then a list with four elements should
      be provided, containing the elements in the exact sequence: a string encoding whether coordinates are towards the 
      northern ('Norte') or southern ('South') hemisphere, followed by the degrees, minutes and seconds.
    proj_out: str
      output projection
    datum_out: str
      output datum
    utm_zone_out: str
      target UTM zone: a number from 11 to 30, encoding the UTM zone for the data, if this was the input projection
    hemisphere: str
      target hemisphere: either 'Norte' or 'Sul'
    """
    # visiting the page and filling in the forms
    self._enter_page()
    print('Filling in the form with the desired inputs and outputs')
    self._fill_first_form(proj_in, datum_in, longitude, latitude)
    self._fill_second_form(proj_in, proj_out, datum_out, utm_zone_out, hemisphere)
    print('Collecting the outputs:')
    contents = self._parse_contents()
    return contents
    
  ## function to close the driver sessions
  def close_driver(self):
    """ 
    Closes the web driver session.
    """
    # closing the web driver sessions if it exists
    try:
      self._driver.close()
      print('Web driver session sucessfully closed!')
    except:
      print('There is not web driver session to be closed!')
    
