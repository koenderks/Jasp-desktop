//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>
#include <boost/filesystem.hpp>

class Utils
{
public:
	enum FileType { jasp = 0, html, csv, txt, sav, ods, pdf, empty, unknown };
	typedef std::vector<Utils::FileType> FileTypeVector;

	static const char* getFileTypeString(const Utils::FileType &fileType);
	static Utils::FileType getTypeFromFileName(const std::string &path);

	static long currentMillis();
	static long currentSeconds();
	static long getFileModificationTime(const std::string &filename);
	static long getFileSize(const std::string &filename);
	static void touch(const std::string &filename);
	static bool renameOverwrite(const std::string &oldName, const std::string &newName);
	static bool removeFile(const std::string &path);

	static boost::filesystem::path osPath(const std::string &path);
	static std::string osPath(const boost::filesystem::path &path);

	static void remove(std::vector<std::string> &target, const std::vector<std::string> &toRemove);
	static void sleep(int ms);

	static const std::string emptyValue;
	static const std::vector<std::string>& getEmptyValues() {return _currentEmptyValues;}
	static const std::vector<std::string>& getDefaultEmptyValues() {return _defaultEmptyValues;}
	static const std::vector<double>& getDoubleEmptyValues() {return _currentDoubleEmptyValues;}
	static void setEmptyValues(const std::vector<std::string>& emptyvalues);

	static bool getIntValue(const std::string& value, int& intValue);
	static bool getIntValue(const double& value, int& intValue);
	static bool getDoubleValue(const std::string& value, double& doubleValue);

private:
	static std::vector<std::string> _currentEmptyValues;
	static const std::vector<std::string> _defaultEmptyValues;
	static std::vector<double> _currentDoubleEmptyValues;
};

#endif // UTILS_H
