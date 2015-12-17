;;; qjp-programming-qml.el --- Settings for QML files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq qmltypes-parser-file-list '("/usr/lib/qt/qml/QtQuick/Controls/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/Dialogs/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/Extras/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/Layouts/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/LocalStorage/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/Particles.2/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/PrivateWidgets/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/Window.2/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick/XmlListModel/plugins.qmltypes"
                                  "/usr/lib/qt/qml/QtQuick.2/plugins.qmltypes"))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-qml))

(provide 'qjp-programming-qml)
;;; qjp-programming-qml.el ends here
